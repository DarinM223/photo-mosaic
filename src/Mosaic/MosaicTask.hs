{-# language BangPatterns #-}
{-# language TypeFamilies #-}

module Mosaic.MosaicTask
    ( calcMosaic
    , CalcResult (..)
    , ImageResult (..)
    ) where

import Codec.Picture
    ( readImage
    , convertRGB8
    , Image (imageWidth, imageHeight)
    , PixelRGB8
    )
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
    ( atomically
    , newTQueue
    , readTQueue
    , writeTQueue
    , TQueue
    )
import Control.Monad (forM_)
import Control.Monad.Identity (runIdentity)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Traversable (forM)
import Data.Vector ((//))
import Mosaic.AvgColor (avgColor, convertPixel, pixelRange)
import Mosaic.KDTree
    ( bulkInitTree
    , nearestNeighbor
    , Dimensional (atDim, dist, numDims, Value)
    , Tree
    )
import Mosaic.Parser (parseInt, parseSpaces, parseQuotes, runParser)

import qualified Data.Vector as V

data CalcResult = CalcResult
    { resultFilename :: String
    , resultAvgColor :: (Int, Int, Int)
    } deriving (Show, Eq)

data ImageResult = ImageResult
    { imageResultWidth     :: Int
    , imageResultHeight    :: Int
    , imageResultFilenames :: [Maybe String]
    } deriving (Show, Eq)

instance Dimensional CalcResult where
    type Value CalcResult = Int

    atDim 0 c = first where (first, _, _) = resultAvgColor c
    atDim 1 c = second where (_, second, _) = resultAvgColor c
    atDim 2 c = third where (_, _, third) = resultAvgColor c
    atDim _ _ = undefined

    dist (CalcResult _ (x1, y1, z1)) other =
        (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2
      where
        x2 = atDim 0 other
        y2 = atDim 1 other
        z2 = atDim 2 other

    numDims _ = 3

calcMosaic :: String -> String -> Int -> Int -> IO (Either String ImageResult)
calcMosaic indexPath imagePath numRows numCols = do
    avgColors <- loadFromFile indexPath
    dynMaybe <- readImage imagePath
    let tree = bulkInitTree avgColors
    case dynMaybe of
        Left err -> return $ Left err
        Right dyn -> do
            results <- processImage tree numRows numCols $ convertRGB8 dyn
            return $ Right results

loadFromFile :: String -> IO [CalcResult]
loadFromFile path = do
    content <- readFile path
    return $ catMaybes $ parseLine <$> lines content

processImage :: Tree CalcResult -> Int -> Int -> Image PixelRGB8 -> IO ImageResult
processImage tree numRows numCols i = do
    q <- atomically newTQueue
    forM_ (zip regions [0..]) $ \(range, n) -> async $ processRegion n q i range tree
    results <- forM regions $ \_ -> atomically $ readTQueue q
    let v = foldl' buildVec (V.replicate (length regions) Nothing) results
    return $ ImageResult w h $ V.toList v
  where
    w = imageWidth i `div` numCols
    h = imageHeight i `div` numRows
    regions = breakRegions w h (imageWidth i) (imageHeight i)
    buildVec v (i, value) = v // [(i, value)]

breakRegions :: Int
             -> Int
             -> Int
             -> Int
             -> [[(Int, Int)]]
breakRegions w h iw ih = go (0, 0) []
  where
    go (!x, !y) build
        | y + h >= ih = reverse build
        | x + w >= iw = go (0, y + h) build
        | otherwise   = go (x + w, y) $ pixelRange (x, y) w h:build

processRegion :: Int
              -> TQueue (Int, Maybe String)
              -> Image PixelRGB8
              -> [(Int, Int)]
              -> Tree CalcResult
              -> IO ()
processRegion i q img range tree =
    atomically $ writeTQueue q (i, imageResult)
  where
    avg = avgColor img convertPixel range
    nearest = nearestNeighbor avg tree
    imageResult = resultFilename <$> nearest

parseLine :: String -> Maybe CalcResult
parseLine = convertToMaybe . runIdentity . runParser go
  where
    convertToMaybe (Left _) = Nothing
    convertToMaybe (Right (result, _)) = Just result
    go = do
        parseSpaces
        s <- parseQuotes '\"'
        parseSpaces
        r <- parseInt
        parseSpaces
        g <- parseInt
        parseSpaces
        b <- parseInt
        return $ CalcResult s (r, g, b)
