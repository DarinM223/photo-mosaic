module Tests (allTests) where

import Test.Tasty
import qualified TreeTest as Tree

allTests :: TestTree
allTests = testGroup "Tests"
    [ Tree.tests
    ]
