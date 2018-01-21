module Main where

import Mosaic.AvgColorTask (calcInDirectory)
import Mosaic.HTMLOutput (generateOutput)
import Mosaic.MosaicTask (calcMosaic)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs ["avg", dirPath, indexPath] =
    calcInDirectory dirPath indexPath
handleArgs ["mosaic", indexPath, imagePath, htmlPath, numRows, numCols] = do
    eitherRes <- calcMosaic indexPath imagePath rows cols
    case eitherRes of
        Left err -> putStrLn $ "Error: " ++ err
        Right res -> writeFile htmlPath . generateOutput rows cols $ res
  where
    rows = read numRows :: Int
    cols = read numCols :: Int
handleArgs _ = do
    putStrLn "Invalid arguments passed"
    putStrLn "Only arguments allowed are: "
    putStrLn "avg [directoryPath] [indexFilePath]"
    putStrLn "mosaic [indexFilePath] [imageFilePath] [htmlOutputPath] [numberOfRows] [numberOfColumns]"
