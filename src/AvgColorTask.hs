module AvgColorTask
    ( calcInDirectory
    ) where

import AvgColor (avgColorOfFile)
import Control.Concurrent.Async (async)
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

calcInDirectory :: String -> String -> IO ()
calcInDirectory path indexPath = do
    files <- getDirectoryContents path
    let imageFiles = fmap appendPath . filter keepImgs $ files
    mapM_ (\f -> async $ getAvgColor f indexPath) imageFiles
  where
    appendPath f = path ++ "/" ++ f
    keepImgs = checkFilename . splitOn "."
    checkFilename words = foldl' (hasImageFilename words) False imageExtensions
    hasImageFilename words isImage ext = isImage || ext `elem` words

getAvgColor :: String -> String -> IO ()
getAvgColor filename indexPath = do
    avgColor <- avgColorOfFile filename
    case avgColor of
        Right color -> writeAvgToFile filename color indexPath
        Left _      -> return ()

writeAvgToFile :: String -> (Int, Int, Int) -> String -> IO ()
writeAvgToFile filename (r, g, b) indexPath = appendFile indexPath s
  where
    s = "\"" ++ filename ++ "\" " ++ show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
