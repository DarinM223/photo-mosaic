{-# language BangPatterns #-}

module Mosaic.AvgColor
    ( avgColor
    , avgColorOfFile
    , convertPixel
    , pixelRange
    ) where

import Codec.Picture
    ( readImage
    , convertRGB8
    , pixelAt
    , Pixel
    , Image (imageWidth, imageHeight)
    , PixelRGB8 (PixelRGB8)
    )
import Data.List (foldl')

-- | Calculate the average color from a stream of RGB values.
avgColor :: (Pixel p)
         => Image p
         -> (p -> (Double, Double, Double))
         -> [(Int, Int)]
         -> (Int, Int, Int)
avgColor img convert = calcAvg . foldl' incAvg ((0.0, 0.0, 0.0), 0.0)
  where
    calcAvg ((r, g, b), num) = (r', g', b')
      where
        r' = round $ r / num :: Int
        g' = round $ g / num :: Int
        b' = round $ b / num :: Int
    incAvg ((!sumR, !sumG, !sumB), !num) (i, j) =
        ((sumR + r, sumG + g, sumB + b), num + 1)
      where
        pixel = pixelAt img i j
        (r, g, b) = convert pixel

-- | Read an image file and calculate the average color.
avgColorOfFile :: String -> IO (Either String (Int, Int, Int))
avgColorOfFile path = do
    dyn <- readImage path
    return $ calcAvgColor . convertRGB8 <$> dyn
  where
    calcAvgColor i = avgColor i convertPixel (range i)
    range i = pixelRange (0, 0) (imageWidth i) (imageHeight i)

pixelRange :: (Int, Int) -> Int -> Int -> [(Int, Int)]
pixelRange (x, y) w h = (\y x -> (x, y)) <$> [y..y + h - 1] <*> [x..x + w - 1]

convertPixel :: PixelRGB8 -> (Double, Double, Double)
convertPixel (PixelRGB8 r g b) = (r', g', b')
  where
    !r' = fromIntegral r :: Double
    !g' = fromIntegral g :: Double
    !b' = fromIntegral b :: Double
