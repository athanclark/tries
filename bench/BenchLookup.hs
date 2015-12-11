{-# LANGUAGE
    FlexibleContexts
  #-}

module Main where

import Build

import Data.Trie.Class as TC
import Criterion.Main


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
