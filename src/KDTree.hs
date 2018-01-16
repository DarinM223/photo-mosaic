module KDTree
    ( insert
    , nearestNeighbor
    , numDims
    , Dimensional
    , Tree (..)
    ) where

class Dimensional d where
    atDim :: Int -> d -> Int

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

data Rectangle = Rectangle
    { topLeft :: (Int, Int)
    , bottomRight :: (Int, Int)
    } deriving (Show, Eq)

numDims :: Int
numDims = 3

insert :: (Dimensional a, Eq a) => a -> Tree a -> Tree a
insert = insertInTree 0

insertInTree :: (Dimensional a, Eq a) => Int -> a -> Tree a -> Tree a
insertInTree _ e Empty = Node e Empty Empty
insertInTree dim e (Node elem left right)
    | e == elem = Node elem left right
    | atDim dim e < atDim dim elem =
        Node elem (insertInTree (incDim dim) e left) right
    | otherwise =
        Node elem left (insertInTree (incDim dim) e right)
  where
    incDim dim = dim + 1 `rem` numDims

nearestNeighbor :: a -> Tree a -> Maybe a
nearestNeighbor = nearestNeighborInTree 0 $ Rectangle (0, 0) (0, 0)

nearestNeighborInTree :: Int -> Rectangle -> a -> Tree a -> Maybe a
nearestNeighborInTree _ _ _ Empty = Nothing
