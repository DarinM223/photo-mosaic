{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

module TreeTest (tests) where

import Test.Tasty
import qualified Test.Tasty.Hedgehog as TH

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.List (sortBy)
import Mosaic.KDTree (balanced, bulkInitTree, nearestNeighbor)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Hedgehog"
    [ TH.testProperty "tree from list is balanced" $ prop_balanced
    , TH.testProperty "nearest neighbor finds nearest neightbor" prop_nearestNeighbor
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" []

genPoint :: Gen (Int, Int, Int)
genPoint = do
    x <- Gen.int $ Range.linear 0 1000
    y <- Gen.int $ Range.linear 0 1000
    z <- Gen.int $ Range.linear 0 1000
    return (x, y, z)

genList :: Gen [(Int, Int, Int)]
genList = Gen.list (Range.linear 0 1000) $ genPoint

prop_balanced :: Property
prop_balanced = property $ forAll genList >>= assert . balanced . bulkInitTree

prop_nearestNeighbor :: Property
prop_nearestNeighbor = property $ do
    p <- forAll genPoint
    l <- forAll genList
    let bruteNearest = bruteForceNN p l
        treeNearest  = nearestNeighbor p . bulkInitTree $ l
    similarResults p bruteNearest treeNearest
  where
    similarResults p (Just p1) (Just p2) = dist p p1 === dist p p2
    similarResults _ Nothing Nothing     = assert True
    similarResults _ _ _                 = assert False

dist :: (Num a) => (a, a, a) -> (a, a, a) -> a
dist (x1, y1, z1) (x2, y2, z2) = (z2 - z1) ^ 2 + (y2 - y1) ^ 2 + (x2 - x1) ^ 2

bruteForceNN :: (Ord a, Num a) => (a, a, a) -> [(a, a, a)] -> Maybe (a, a, a)
bruteForceNN p = toMaybe . sortBy compareDists . map (pointWithDist p)
  where
    compareDists (_, a) (_, b) = a `compare` b
    pointWithDist other p = (p, dist p other)

    toMaybe [] = Nothing
    toMaybe (p:_) = Just $ fst p

