module Mosaic.HTMLOutput
    ( generateOutput
    ) where

import Mosaic.MosaicTask (ImageResult (ImageResult))

htmlImg :: Maybe String -> Int -> Int -> String
htmlImg (Just filename) w h = "<img src=\"" ++ filename ++
    "\" width=\"" ++ show w ++ "\" height=\"" ++ show h ++ "\">"
htmlImg Nothing w h =
    "<img src=\"data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs=\" width=\"" ++
        show w ++ "\" height=\"" ++ show h ++ "\" />"

htmlBr :: String
htmlBr = "<br>"

generateOutput :: Int -> ImageResult -> String
generateOutput numCols (ImageResult w h filenames) =
    go 0 "" filenames
  where
    go _ build [] = build
    go i build filenames@(f:rest)
        | i >= numCols = go 0 (build ++ htmlBr) filenames
        | otherwise    = go (i + 1) (build ++ htmlImg f w h) rest
