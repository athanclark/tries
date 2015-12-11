{-# LANGUAGE
    FlexibleContexts
  #-}

module Main where

import Build

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Tree (Tree (..))
import Data.Trie.List              as L
import Data.Trie.Map               as M
import Data.Trie.HashMap           as HM
import Data.Tree.Knuth.Forest      as K
import Data.Trie.Knuth             as K
import Data.Trie.Class             as TC
import qualified Data.Map          as Map
import qualified Data.HashMap.Lazy as HMap
import Criterion.Main
import Control.Monad.State



deleteMany :: ( Trie NonEmpty Int c
              ) => Int -> c Int a -> c Int a
deleteMany top xs =
  foldr
    (\p -> TC.delete $ buildMiddleQuery top)
    xs
    [1..top]

insertMany :: ( Trie NonEmpty Int c
              ) => Int -> c Int Int -> c Int Int
insertMany top xs =
  foldr
    (\p -> TC.insert (buildMiddleQuery top) 0)
    xs
    [1..top]


main = defaultMain
  [ bgroup "delete"
    [ bench "ListTrie"    $ whnf (deleteMany 100) (genListTrie    100)
    , bench "MapTrie"     $ whnf (deleteMany 100) (genMapTrie     100)
    , bench "HashMapTrie" $ whnf (deleteMany 100) (genHashMapTrie 100)
    , bench "KnuthTrie"   $ whnf (deleteMany 100) (genKnuthTrie   100)
    ]
  , bgroup "insert"
    [ bench "ListTrie"    $ whnf (insertMany 100) (genListTrie    100)
    , bench "MapTrie1"    $ whnf (insertMany 100) (genMapTrie     100)
    , bench "HashMapTrie" $ whnf (insertMany 100) (genHashMapTrie 100)
    , bench "KnuthTrie"   $ whnf (insertMany 100) (genKnuthTrie   100)
    ]
  ]
