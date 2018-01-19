{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Mosaic.Iterator
    ( createImageIter
    , createRangeIter
    , runIteratorT
    , ImageIterator
    , IteratorM (..)
    , IteratorT
    ) where

import Control.Monad.State (StateT, get, modify, runStateT)
import Codec.Picture (Image (..), Pixel, pixelAt)
import Data.List (foldl')

class (Monad m) => IteratorM p m where
    next :: (p -> a) -> m (Maybe a)

data ImageIterator p = ImageIterator
    { imageMaxWidth  :: Int
    , imageMaxHeight :: Int
    , imageCurrPos   :: (Int, Int)
    , imageStartPos  :: (Int, Int)
    , image          :: Image p
    }

createImageIter :: Image p -> ImageIterator p
createImageIter image =
    ImageIterator (imageWidth image) (imageHeight image) (0, 0) (0, 0) image

createRangeIter :: Int -> Int -> (Int, Int) -> Image p -> ImageIterator p
createRangeIter w h = ImageIterator w h (0, 0)

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

getPixel :: (Pixel p) => ImageIterator p -> p
getPixel iter = pixelAt (image iter) x y
  where
    x = fst . imageCurrPos $ iter
    y = snd . imageCurrPos $ iter

-- Returns the relative x position in the range.
xPos :: ImageIterator p -> Int
xPos i = (fst . imageCurrPos) i - (fst . imageStartPos) i

-- Returns the relative y position in the range.
yPos :: ImageIterator p -> Int
yPos i = (snd . imageCurrPos) i - (snd . imageStartPos) i

atEnd :: ImageIterator p -> Bool
atEnd i
    | ry >= h   = True
    | otherwise = False
  where
    ry = yPos i
    h = imageMaxHeight i

updateIter :: ImageIterator p -> ImageIterator p
updateIter i
    | rx >= w - 1 = i { imageCurrPos = (sx, y + 1) }
    | otherwise   = i { imageCurrPos = (x + 1, y) }
  where
    rx = xPos i
    w = imageMaxWidth i
    (x, y) = imageCurrPos i
    (sx, _) = imageStartPos i
