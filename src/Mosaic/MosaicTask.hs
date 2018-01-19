module Mosaic.MosaicTask
    ( calcMosaic
    , breakRegions
    , CalcResult (..)
    , ImageResult (..)
    ) where

import Codec.Picture (readImage, convertRGB8, Image (..), PixelRGB8 (..))
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
import Mosaic.KDTree (bulkInitTree, nearestNeighbor, Dimensional (..), Tree)
import Mosaic.Iterator (createRangeIter, runIteratorT, ImageIterator)
import Mosaic.Parser (parseInt, parseSpaces, parseQuotes, runParser)

import qualified Data.Vector as V
import qualified Mosaic.AvgColor as Avg

data CalcResult = CalcResult
    { filename :: String
    , avgColor :: (Int, Int, Int)
    } deriving (Show, Eq)

data ImageResult = ImageResult
    { imageResultWidth     :: Int
    , imageResultHeight    :: Int
    , imageResultFilenames :: [Maybe String]
    } deriving (Show, Eq)

instance Dimensional CalcResult where
    atDim 0 c = first where (first, _, _) = avgColor c
    atDim 1 c = second where (_, second, _) = avgColor c
    atDim 2 c = third where (_, _, third) = avgColor c

    dist (CalcResult _ (x1, y1, z1)) (CalcResult _ (x2, y2, z2)) =
        (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

loadFromFile :: String -> IO [CalcResult]
loadFromFile path = do
    content <- readFile path
    return $ catMaybes $ parseLine <$> lines content

breakRegions :: Int -> Int -> Int -> Int -> Image PixelRGB8 -> [ImageIterator PixelRGB8]
breakRegions w h iw ih i = go (0, 0) []
  where
    go (x, y) build
        | x + w >= iw && y + h >= ih = reverse build
        | x + w >= iw                = go (0, y + h) $ newIter (x, y):build
        | otherwise                  = go (x + w, y) $ newIter (x, y):build
    newIter p = createRangeIter w h p i

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

processImage :: Tree CalcResult -> Int -> Int -> Image PixelRGB8 -> IO ImageResult
processImage tree numRows numCols i = do
    q <- atomically newTQueue
    forM_ (zip regions [0..]) $ \(iter, i) -> async $ processRegion i q iter tree
    results <- forM regions $ \_ -> atomically $ readTQueue q
    let v = foldl' buildVec (V.replicate (length regions) Nothing) results
    return $ ImageResult w h $ V.toList v
  where
    w = imageWidth i `div` numCols
    h = imageHeight i `div` numRows
    regions = breakRegions w h (imageWidth i) (imageHeight i) i
    buildVec v (i, value) = v // [(i, value)]

processRegion :: Int -> TQueue (Int, Maybe String) -> ImageIterator PixelRGB8 -> Tree CalcResult -> IO ()
processRegion i q iter tree = do
    avg <- runIteratorT (Avg.avgColor Avg.convertPixel) iter
    let avgCalcRes  = CalcResult "" avg
        nearest     = nearestNeighbor avgCalcRes tree
        imageResult = filename <$> nearest
    atomically $ writeTQueue q (i, imageResult)

parseLine :: String -> Maybe CalcResult
parseLine = convertToMaybe . runIdentity . runParser go
  where
    convertToMaybe (Left err) = Nothing
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
