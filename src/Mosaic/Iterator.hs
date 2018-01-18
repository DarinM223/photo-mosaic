{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Mosaic.Iterator
    ( createImageIter
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
    , image          :: Image p
    }

createImageIter :: Image p -> ImageIterator p
createImageIter image =
    ImageIterator (imageWidth image) (imageHeight image) (0, 0) image

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

atEnd :: ImageIterator p -> Bool
atEnd i@(ImageIterator { imageCurrPos = (x, y) })
    | x >= imageMaxWidth i || y >= imageMaxHeight i = True
    | otherwise                                     = False

updateIter :: ImageIterator p -> ImageIterator p
updateIter i@(ImageIterator { imageCurrPos = (x, y) })
    | x == imageMaxWidth i = i { imageCurrPos = (0, y + 1) }
    | otherwise            = i { imageCurrPos = (x + 1, y) }
