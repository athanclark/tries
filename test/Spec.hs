module Spec where

import Data.TrieSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [spec]
