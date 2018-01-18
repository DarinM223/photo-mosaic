module Mosaic.AvgColor
    ( avgColor
    , avgColorOfFile
    ) where

import Codec.Picture (readImage, convertRGB8, PixelRGB8 (..))
import Mosaic.Iterator (IteratorM (next), createImageIter, runIteratorT)

-- | Calculate the average color from a stream of RGB values.
avgColor :: (IteratorM p m, Monad m)
         => (p -> (Double, Double, Double))
         -> m (Int, Int, Int)
avgColor convert = avgColorRec convert ((0.0, 0.0, 0.0), 0.0)

avgColorRec :: (IteratorM p m, Monad m)
            => (p -> (Double, Double, Double))
            -> ((Double, Double, Double), Double)
            -> m (Int, Int, Int)
avgColorRec convert ((sumR, sumG, sumB), num) = do
    result <- next convert
    case result of
        Just (r, g, b) ->
            let state' = ((sumR + r, sumG + g, sumB + b), num + 1)
            in avgColorRec convert state'
        Nothing ->
            let r'  = round $ sumR / num :: Int
                g'  = round $ sumG / num :: Int
                b'  = round $ sumB / num :: Int
                avg = (r', g', b')
            in return avg

-- | Read an image file and calculate the average color.
avgColorOfFile :: String -> IO (Either String (Int, Int, Int))
avgColorOfFile path = do
    dynMaybe <- readImage path
    case dynMaybe of
        Left err -> return $ Left err
        Right dyn -> do
            let image = convertRGB8 dyn
                iter  = createImageIter image
            (avg, _) <- runIteratorT (avgColor convertPixel) iter
            return $ Right avg

convertPixel :: PixelRGB8 -> (Double, Double, Double)
convertPixel (PixelRGB8 r g b) = (r', g', b')
  where
    r' = fromIntegral r :: Double
    g' = fromIntegral g :: Double
    b' = fromIntegral b :: Double
