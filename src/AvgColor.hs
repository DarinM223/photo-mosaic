{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AvgColor
    ( avgColor
    , avgColorOfFile
    ) where

import Control.Monad.State (StateT, get, modify, runStateT)
import Data.List (foldl')
import Codec.Picture (Image (..), readImage, convertRGB8, Pixel, PixelRGB8 (..), pixelAt)

class (Monad m) => IteratorM p m where
    next :: (p -> a) -> m (Maybe a)

data ImageIterator p = ImageIterator
    { imageMaxWidth  :: Int
    , imageMaxHeight :: Int
    , imageCurrPos   :: (Int, Int)
    , image          :: Image p
    }

createImageIter :: Image p -> ImageIterator p
createImageIter image =
    ImageIterator (imageWidth image) (imageHeight image) (0, 0) image

getPixel :: (Pixel p) => ImageIterator p -> p
getPixel iter = pixelAt (image iter) x y
  where
    x = fst . imageCurrPos $ iter
    y = snd . imageCurrPos $ iter

atEnd :: ImageIterator p -> Bool
atEnd i@(ImageIterator { imageCurrPos = (x, y) })
    | x >= imageMaxWidth i || y >= imageMaxHeight i = True
    | otherwise                                     = False

updateIter :: ImageIterator p -> ImageIterator p
updateIter i@(ImageIterator { imageCurrPos = (x, y) })
    | x == imageMaxWidth i = i { imageCurrPos = (0, y + 1) }
    | otherwise            = i { imageCurrPos = (x + 1, y) }

newtype IteratorT p m a = IteratorT { fromIteratorT :: StateT (ImageIterator p) m a }
    deriving (Functor, Applicative, Monad)

runIteratorT :: IteratorT p m a -> ImageIterator p -> m (a, ImageIterator p)
runIteratorT = runStateT . fromIteratorT

instance (Functor m, Monad m, Pixel p) => IteratorM p (IteratorT p m) where
    next convert = IteratorT $ do
        iter <- get
        if atEnd iter
            then return Nothing
            else do
                pixel <- getPixel <$> get
                modify updateIter
                return $ Just $ convert pixel

-- | Calculate the average color from a stream of RGB values.
avgColor :: (IteratorM p m, Monad m)
         => (p -> (Int, Int, Int))
         -> m (Int, Int, Int)
avgColor convert = avgColorRec convert ((0.0, 0.0, 0.0), 0.0)

avgColorRec :: (IteratorM p m, Monad m)
            => (p -> (Int, Int, Int))
            -> ((Double, Double, Double), Double)
            -> m (Int, Int, Int)
avgColorRec convert ((sumR, sumG, sumB), num) = do
    result <- next convert
    case result of
        Just (r, g, b) ->
            let r'     = sumR + fromIntegral r
                g'     = sumG + fromIntegral g
                b'     = sumB + fromIntegral b
                state' = ((r', g', b'), num + 1)
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

convertPixel :: PixelRGB8 -> (Int, Int, Int)
convertPixel (PixelRGB8 r g b) = (r', g', b')
  where
    r' = fromIntegral r :: Int
    g' = fromIntegral g :: Int
    b' = fromIntegral b :: Int
