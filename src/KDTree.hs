module KDTree
    ( insert
    , nearestNeighbor
    , numDims
    , Dimensional
    , Tree (..)
    ) where

import Data.List (foldl')

class Dimensional d where
    atDim :: Int -> d -> Int
    dist  :: d -> d -> Int

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

numDims :: Int
numDims = 3

bulkInsert :: (Dimensional a, Eq a) => [a] -> Tree a -> Tree a
-- TODO(DarinM223): make tree more balanced with sorted list
bulkInsert elems tree = foldl' (flip insert) tree elems

insert :: (Dimensional a, Eq a) => a -> Tree a -> Tree a
insert = insertInTree 0

incDim :: Int -> Int
incDim dim = dim + 1 `rem` numDims

insertInTree :: (Dimensional a, Eq a) => Int -> a -> Tree a -> Tree a
insertInTree _ e Empty = Node e Empty Empty
insertInTree dim e (Node elem left right)
    | e == elem = Node elem left right
    | atDim dim e < atDim dim elem =
        Node elem (insertInTree (incDim dim) e left) right
    | otherwise =
        Node elem left (insertInTree (incDim dim) e right)

nearestNeighbor :: (Dimensional a, Eq a) => a -> Tree a -> Maybe a
nearestNeighbor = nearestNeighborInTree 0

nearestNeighborInTree :: (Dimensional a, Eq a) => Int -> a -> Tree a -> Maybe a
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
        searchTree = nearestNeighborInTree (incDim dim) searchP

        -- Compares the nearest neighbor result to another point
        -- by comparing their distance to the search point.
        bestPoint compare Nothing = compare
        bestPoint compare (Just best)
            | dist best searchP < dist compare searchP = best
            | otherwise                                = compare

        -- TODO(DarinM223): understand how this works better
        sphereIntersectsPlane closest =
            (searchPDim - currPDim) ^ 2 <= dist currP closest
