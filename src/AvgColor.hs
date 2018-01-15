{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AvgColor
    ( avgColor
    , avgColorOfFile
    ) where

import Control.Monad.State (StateT, get, modify)
import Data.List (foldl')
import Codec.Picture (Image, readImage, convertRGB8, Pixel, PixelRGB8 (..), pixelAt)

data ImageIterator p = ImageIterator
    { imageMaxWidth  :: Int
    , imageMaxHeight :: Int
    , imageCurrPos   :: (Int, Int)
    , image          :: Image p
    }

class (Monad m) => IteratorM p m where
    next :: (p -> a) -> m (Maybe a)

newtype IteratorT p m a = IteratorT { fromIteratorT :: StateT (ImageIterator p) m a }
    deriving (Functor, Applicative, Monad)

instance (Functor m, Monad m, Pixel p) => IteratorM p (IteratorT p m) where
    next convert = IteratorT $ do
        iter <- get
        if atEnd iter
            then return Nothing
            else do
                pixel <- getPixel <$> get
                modify updateIter
                return $ Just $ convert pixel

getPixel :: (Pixel p) => ImageIterator p -> p
getPixel iter = pixelAt (image iter) x y
  where
    x = fst . imageCurrPos $ iter
    y = snd . imageCurrPos $ iter

atEnd :: ImageIterator p -> Bool
atEnd i
    | imageCurrPos i == (imageMaxWidth i, imageMaxHeight i) = True
    | otherwise                                             = False

updateIter :: ImageIterator p -> ImageIterator p
updateIter iter = iter

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
            let r' = sumR + fromIntegral r
                g' = sumG + fromIntegral g
                b' = sumB + fromIntegral b
                state' = ((r', g', b'), num + 1)
            in avgColorRec convert state'
        Nothing ->
            let r' = round $ sumR / num :: Int
                g' = round $ sumG / num :: Int
                b' = round $ sumB / num :: Int
                avg = (r', g', b')
            in return avg

-- | Read an image file and calculate the average color.
avgColorOfFile :: String -> IO (Int, Int, Int)
avgColorOfFile path = do
    Right dyn <- readImage path
    let image = convertRGB8 dyn
    return (1, 1, 1)
