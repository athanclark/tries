module Data.Trie.List where

import Prelude hiding (lookup)
import qualified Data.Tree as T
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE


lookup :: NonEmpty t -> T.Tree (t, Maybe x) -> Maybe x
lookup (t:ts) (Node (t',mx) xs) = do
  guard $ t == t'
  if null ts then mx
             else lookup (NE.fromList ts) xs
