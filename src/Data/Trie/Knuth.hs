{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Data.Trie.Knuth where

import Prelude hiding (lookup)
import           Data.Tree.Knuth.Forest (KnuthForest (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Trie.Class (Trie (..))

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary)


newtype KnuthTrie s x = KnuthTrie
  { unKnuthTrie :: KnuthForest (s, Maybe x)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary, Generic, Data, Typeable)

instance ( NFData s
         , NFData x
         ) => NFData (KnuthTrie s x)

instance Eq s => Trie NonEmpty s KnuthTrie where
  lookup _ (KnuthTrie Nil) = Nothing
  lookup tss@(t:|ts) (KnuthTrie (Fork (t',mx) cs ss))
    | t == t' = if null ts
                then mx
                else lookup (NE.fromList ts) $ KnuthTrie cs
    | otherwise = lookup tss $ KnuthTrie ss

  insert (t:|ts) x (KnuthTrie Nil)
    | null ts = KnuthTrie $ Fork (t,Just x) Nil Nil
    | otherwise = let cs' = unKnuthTrie $ insert (NE.fromList ts) x $ KnuthTrie Nil
                  in  KnuthTrie $ Fork (t,Nothing) cs' Nil
  insert tss@(t:|ts) x (KnuthTrie (Fork s@(t',_) cs ss))
    | t == t' = if null ts
                then KnuthTrie $ Fork (t',Just x) cs ss
                else let cs' = unKnuthTrie $ insert (NE.fromList ts) x $ KnuthTrie cs
                     in  KnuthTrie $ Fork s cs' ss
    | otherwise = KnuthTrie $ Fork s cs $ unKnuthTrie $ insert tss x $ KnuthTrie ss

  delete _ xs@(KnuthTrie Nil) = xs
  delete tss@(t:|ts) (KnuthTrie (Fork s@(t',_) cs ss))
    | t == t' = if null ts
                then KnuthTrie $ Fork (t',Nothing) cs ss
                else let cs' = unKnuthTrie $ delete (NE.fromList ts) $ KnuthTrie cs
                     in  KnuthTrie $ Fork s cs' ss
    | otherwise = KnuthTrie $ Fork s cs $ unKnuthTrie $ delete tss $ KnuthTrie ss
