{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}

module Mosaic.KDTree
    ( bulkInitTree
    , insert
    , nearestNeighbor
    , Dimensional (..)
    , Tree (..)
    ) where

import Data.List (sortBy)

class (Eq d) => Dimensional d where
    -- | Returns the value at the given axis (starting at 0).
    atDim :: Int -> d -> Int
    -- | Returns the squared distance between two Dimensionals.
    dist  :: (Dimensional e) => d -> e -> Int
    -- | Returns the number of axises in the Dimensional.
    numDims :: d -> Int

instance Dimensional (Int, Int, Int) where
    atDim 0 (first, _, _) = first
    atDim 1 (_, second, _) = second
    atDim 2 (_, _, third) = third

    dist (x1, y1, z1) other =
        (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2
      where
        x2 = atDim 0 other
        y2 = atDim 1 other
        z2 = atDim 2 other

    numDims _ = 3

incDim :: forall d. (Dimensional d) => Int -> d -> Int
incDim dim d = (dim + 1) `rem` numDims d

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

bulkInitTree :: (Dimensional a) => [a] -> Tree a
bulkInitTree = bulkInitDim 0

bulkInitDim :: forall a. (Dimensional a) => Int -> [a] -> Tree a
bulkInitDim dim [e] = Node e Empty Empty
bulkInitDim dim [] = Empty
bulkInitDim dim l =
    Node mid (bulkInitDim nextDim left) (bulkInitDim nextDim right)
  where
    nextDim = incDim dim (undefined :: a)
    (left, mid:right) = splitAt midIdx sortedList
    sortedList = sortBy (compareDim dim) l
    compareDim dim a b = atDim dim a `compare` atDim dim b
    midIdx = length l `div` 2

insert :: (Dimensional a) => a -> Tree a -> Tree a
insert = insertInTree 0

insertInTree :: forall a. (Dimensional a) => Int -> a -> Tree a -> Tree a
insertInTree _ e Empty = Node e Empty Empty
insertInTree dim e (Node elem left right)
    | e == elem = Node elem left right
    | atDim dim e < atDim dim elem =
        Node elem (insertInTree nextDim e left) right
    | otherwise =
        Node elem left (insertInTree nextDim e right)
  where
    nextDim = incDim dim (undefined :: a)

nearestNeighbor :: forall a b. (Dimensional a, Dimensional b)
                => b
                -> Tree a
                -> Maybe a
nearestNeighbor = nearestNeighborInTree 0

nearestNeighborInTree :: forall a b. (Dimensional a, Dimensional b)
                      => Int
                      -> b
                      -> Tree a
                      -> Maybe a
nearestNeighborInTree _ _ Empty = Nothing
nearestNeighborInTree _ _ (Node e Empty Empty) = Just e
nearestNeighborInTree dim searchP (Node currP left right)
    | searchPDim < currPDim = go left right
    | otherwise             = go right left
  where
    searchPDim = atDim dim searchP
    currPDim = atDim dim currP
    go t1 t2 = Just $
        if sphereIntersectsPlane closestT1
            then bestPoint closestT1 . searchTree $ t2
            else closestT1
      where
        closestT1 = bestPoint currP . searchTree $ t1

        -- Searches and returns the nearest neighbor.
        nextDim = incDim dim (undefined :: a)
        searchTree = nearestNeighborInTree nextDim searchP

        -- Compares the nearest neighbor result to another point
        -- by comparing their distance to the search point.
        bestPoint compare Nothing = compare
        bestPoint compare (Just best)
            | dist best searchP < dist compare searchP = best
            | otherwise                                = compare

        -- TODO(DarinM223): understand how this works better
        sphereIntersectsPlane closest =
            (searchPDim - currPDim) ^ 2 <= dist currP closest
