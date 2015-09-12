module Data.TrieSpec (spec) where

import Data.Trie

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Data.Trie"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
