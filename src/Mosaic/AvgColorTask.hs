module Mosaic.AvgColorTask
    ( calcInDirectory
    ) where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM
    ( atomically
    , newTQueue
    , readTQueue
    , writeTQueue
    , TQueue
    )
import Data.Foldable (forM_)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Mosaic.AvgColor (avgColorOfFile)
import Mosaic.MosaicTask (CalcResult (resultFilename, CalcResult))
import System.Directory (getDirectoryContents)

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

    q <- atomically newTQueue
    mapM_ (async . handleAvg q) imageFiles
    forM_ imageFiles $ \_ -> do
        result <- atomically $ readTQueue q
        forM_ result $ \result -> do
            putStrLn $ show (resultFilename result) ++ " finished"
            writeResultToFile result indexPath
  where
    appendPath f = path ++ "/" ++ f
    keepImgs = checkFilename . splitOn "."
    checkFilename words = foldl' (hasImageFilename words) False imageExtensions
    hasImageFilename words isImage ext = isImage || ext `elem` words

handleAvg :: TQueue (Maybe CalcResult) -> String -> IO ()
handleAvg q filename = avgColorOfFile filename >>= \case
    Right color -> atomically $ writeTQueue q $ Just $ CalcResult filename color
    Left _      -> atomically $ writeTQueue q Nothing

writeResultToFile :: CalcResult -> String -> IO ()
writeResultToFile (CalcResult filename (r, g, b)) indexPath = appendFile indexPath s
  where
    s = "\"" ++ filename ++ "\" " ++ show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
