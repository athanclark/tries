{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  #-}

module Data.Trie.List where

import Prelude hiding (lookup)
import           Data.Tree (Tree (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Key hiding (lookup)
import Data.Monoid
import Control.Monad


newtype ListTrie t x = ListTrie
  { unListTrie :: Tree (t, Maybe x)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type instance Key (ListTrie t) = NonEmpty t
-- TODO: Trie instance



lookup :: Eq t => NonEmpty t -> ListTrie t x -> Maybe x
lookup (t:|ts) (ListTrie (Node (t',mx) xs)) = do
  guard $ t == t'
  if null ts then mx
             else getFirst $ foldMap (First . lookup (NE.fromList ts) . ListTrie) xs
