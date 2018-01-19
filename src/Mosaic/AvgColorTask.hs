module Mosaic.AvgColorTask
    ( calcInDirectory
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTQueue, readTQueue, writeTQueue, TQueue)
import Control.Monad (forever)
import Data.Foldable (forM_)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import GHC.Conc (getNumProcessors)
import Mosaic.AvgColor (avgColorOfFile)
import Mosaic.MosaicTask (CalcResult (..))
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
    numCores <- getNumProcessors
    let imageFiles = fmap appendPath . filter keepImgs $ files

    inQ <- atomically newTQueue
    outQ <- atomically newTQueue

    forM_ imageFiles $ \f -> atomically $ writeTQueue inQ f
    forM_ [1..numCores * 2] $ \w -> forkIO $ worker w inQ outQ
    forM_ imageFiles $ \_ -> do
        result <- atomically $ readTQueue outQ
        case result of
            Just result -> do
                putStrLn $ show (filename result) ++ " finished"
                writeResultToFile result indexPath
            Nothing     -> return ()
  where
    appendPath f = path ++ "/" ++ f
    keepImgs = checkFilename . splitOn "."
    checkFilename words = foldl' (hasImageFilename words) False imageExtensions
    hasImageFilename words isImage ext = isImage || ext `elem` words

worker :: Int -> TQueue String -> TQueue (Maybe CalcResult) -> IO ()
worker n inQ outQ = forever $ do
    filename <- atomically $ readTQueue inQ
    avgColor <- avgColorOfFile filename
    case avgColor of
        Right color -> atomically $ writeTQueue outQ $ Just $ CalcResult filename color
        Left _      -> atomically $ writeTQueue outQ Nothing

writeResultToFile :: CalcResult -> String -> IO ()
writeResultToFile (CalcResult filename (r, g, b)) indexPath = appendFile indexPath s
  where
    s = "\"" ++ filename ++ "\" " ++ show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
