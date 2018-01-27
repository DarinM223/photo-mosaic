{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List (sort, sortBy)
import Mosaic.KDTree (balanced, bulkInitTree, nearestNeighbor)
import Debug.Trace

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "QuickCheck"
    [ QC.testProperty "tree from list is balanced" $
        \list -> balanced . bulkInitTree $ (list :: [(Int, Int, Int)])
    , QC.testProperty "nearest neighbor finds nearest neighbor" testNearestNeighbor
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" []

testNearestNeighbor :: (NonNegative Int, NonNegative Int, NonNegative Int)
                    -> [(NonNegative Int, NonNegative Int, NonNegative Int)]
                    -> Bool
testNearestNeighbor (NonNegative a, NonNegative b, NonNegative c) nnList =
    traceResult p bruteNearest treeNearest $ similarResults p bruteNearest treeNearest
  where
    p = (a, b, c)
    list = (\(NonNegative a, NonNegative b, NonNegative c) -> (a, b, c)) <$> nnList

    dist (x1, y1, z1) (x2, y2, z2) = (z2 - z1) ^ 2 + (y2 - y1) ^ 2 + (x2 - x1) ^ 2
    compareDists (_, a) (_, b) = a `compare` b
    pointWithDist other p = (p, dist p other)

    toMaybe [] = Nothing
    toMaybe (p:_) = Just $ fst p

    bruteForceNN p = toMaybe . sortBy compareDists . map (pointWithDist p)

    bruteNearest = bruteForceNN p list
    treeNearest = nearestNeighbor p . bulkInitTree $ list

    similarResults p (Just p1) (Just p2) = dist p p1 == dist p p2
    similarResults _ Nothing Nothing = True
    similarResults _ _ _ = False

    traceResult p p1 p2 False =
        trace ("P: " ++ show p ++ " Brute Nearest: " ++ show p1 ++ " Tree Nearest: " ++ show p2) False
    traceResult _ _ _ True = True
