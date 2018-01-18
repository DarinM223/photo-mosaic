module Mosaic.MosaicTask
    ( loadFromFile
    ) where

import Control.Monad (forM_)
import Mosaic.KDTree (bulkInitTree, Dimensional (..))

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
    forM_ (lines content) $ \l -> putStrLn l
    return []
