module AvgColorTask
    ( calcInDirectory
    , writeAvgsToFile
    ) where

import AvgColor (avgColorOfFile)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
    ( atomically
    , TQueue
    , newTQueue
    , writeTQueue
    , readTQueue
    )
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Traversable (forM)
import Data.Maybe (catMaybes)
import System.Directory (getDirectoryContents)

data CalcResult = CalcResult
    { filename :: String
    , avg      :: (Int, Int, Int)
    } deriving (Show)

imageExtensions :: [String]
imageExtensions =
    [ "jpg"
    , "jpeg"
    , "png"
    , "gif"
    ]

calcInDirectory :: String -> IO [CalcResult]
calcInDirectory path = do
    files <- getDirectoryContents path
    let imageFiles = fmap appendPath . filter keepImgs $ files
    c <- atomically newTQueue
    mapM_ (\f -> async $ getAvgColor f c) imageFiles
    results <- mapM (\_ -> atomically $ readTQueue c) imageFiles
    return $ catMaybes results
  where
    appendPath f = path ++ "/" ++ f
    keepImgs = checkFilename . splitOn "."
    checkFilename words = foldl' (hasImageFilename words) False imageExtensions
    hasImageFilename words isImage ext = isImage || ext `elem` words

getAvgColor :: String -> TQueue (Maybe CalcResult) -> IO ()
getAvgColor filename chan = do
    avgColor <- avgColorOfFile filename
    atomically $ writeTQueue chan $ makeCalcResult avgColor filename
  where
    makeCalcResult (Right color) filename = Just (CalcResult filename color)
    makeCalcResult (Left _) filename      = Nothing

writeAvgsToFile :: String -> [CalcResult] -> IO ()
writeAvgsToFile filename results = return ()
