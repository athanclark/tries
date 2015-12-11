{-# LANGUAGE
    FlexibleContexts
  #-}

module Main where

import Build

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Tree (Tree (..))
import Data.Trie.List as L
import Data.Trie.Map as M
import Data.Trie.HashMap as HM
import Data.Tree.Knuth.Forest as K
import Data.Trie.Knuth as K
import Data.Trie.Class as TC
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HMap
import Criterion.Main
import Control.Monad.State


main = defaultMain
  [ bgroup "lookup"
    [ bgroup "ListTrie"    $ benchOne genListTrie    <$> [10,20..100]
    , bgroup "MapTrie"     $ benchOne genMapTrie     <$> [10,20..100]
    , bgroup "HashMapTrie" $ benchOne genHashMapTrie <$> [10,20..100]
    , bgroup "KnuthTrie"   $ benchOne genKnuthTrie   <$> [10,20..100]
    ]
  ]
  where
    benchOne gen x = bench (show x) $ whnf (TC.lookup $ buildMiddleQuery x)  (gen 100)
