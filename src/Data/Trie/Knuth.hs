module Data.Trie.Knuth where

import Prelude hiding (lookup)
import           Data.Tree.Knuth (KnuthForest (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE


type KnuthTrie t x = KnuthForest (t, Maybe x)

lookup :: Eq t => NonEmpty t -> KnuthTrie t x -> Maybe x
lookup _ Nil = Nothing
lookup (t:|ts) (Fork (t',mx) cs ss) | t == t' =
  if null ts then mx
             else lookup (NE.fromList ts) cs
                                    | otherwise = lookup (t:|ts) ss
