{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Data.Trie.List where

import Data.Trie.Class

import Prelude hiding (lookup)
import           Data.Tree (Tree (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Maybe (fromMaybe)
import Data.Key hiding (lookup)
import Data.Monoid
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Instances


newtype ListTrie t x = ListTrie
  { unListTrie :: Tree (t, Maybe x)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

type instance Key (ListTrie s) = NonEmpty s
-- TODO: Trie instance


instance Eq s => Trie NonEmpty s ListTrie where
  lookup (t:|ts) (ListTrie (Node (t',mx) xs)) = do
    guard $ t == t'
    if null ts then mx
               else getFirst $ foldMap (First . lookup (NE.fromList ts) . ListTrie) xs

  delete (t:|ts) xss@(ListTrie (Node (t',mx) xs)) =
    fromMaybe xss $ do
      guard $ t == t'
      return $ if null ts
               then ListTrie $ Node (t',Nothing) xs
               else ListTrie $ Node (t',mx) $ fmap go xs
    where go = unListTrie . delete (NE.fromList ts) . ListTrie

  insert (t:|ts) x yss@(ListTrie ys) =
    fromMaybe yss $ go ys
    where
      go (Node (t',mx) xs) = do
        guard $ t == t'
        return $ if null ts
                 then ListTrie $ Node (t',Just x) xs
                 else ListTrie $ Node (t',mx) $
                    if any (hasHead $ head ts) xs
                    then fmap (unListTrie . insert (NE.fromList ts) x . ListTrie) xs
                    else xs ++ [unListTrie $ insert (NE.fromList ts) x $ ListTrie $
                                  Node (head ts, Nothing) []]
      hasHead t (Node (t',_) _) = t == t'
