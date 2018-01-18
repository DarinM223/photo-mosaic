module Mosaic.MosaicTask
    ( loadFromFile
    ) where

import Control.Monad (forM_)
import Control.Monad.Identity (runIdentity)
import Data.Maybe (catMaybes)
import Mosaic.KDTree (bulkInitTree, Dimensional (..))
import Mosaic.Parser (parseInt, parseSpaces, parseQuotes, runParser)

data CalcResult = CalcResult
    { filename :: String
    , avgColor :: (Int, Int, Int)
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
