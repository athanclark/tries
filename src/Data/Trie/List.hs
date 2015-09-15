module Data.Trie.List where

import Prelude hiding (lookup)
import           Data.Trie.List.Internal
import           Data.Tree (Tree (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Monoid
import Control.Monad


lookup :: Eq t => NonEmpty t -> ListTrie t x -> Maybe x
lookup (t:|ts) (ListTrie (Node (t',mx) xs)) = do
  guard $ t == t'
  if null ts then mx
             else getFirst $ foldMap (First . lookup (NE.fromList ts) . ListTrie) xs
