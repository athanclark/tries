{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  #-}

module Data.Trie.List.Internal where

import Data.Tree (Tree (..))

import Data.Key


newtype ListTrie t x = ListTrie
  { unListTrie :: Tree (t, Maybe x)
  } deriving (Show, Eq, Functor)

type instance Key (ListTrie t) = t
