module Data.Trie.Map where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE


newtype MapTrie t x = MapTrie
  { unMapTrie :: Map.Map t (Maybe x, Maybe (MapTrie t x)) }

lookup :: Ord t => NonEmpty t -> MapTrie t x -> Maybe x
lookup (t:|ts) (MapTrie xs) = do
  (mx,mxs) <- Map.lookup t xs
  if null ts then mx
             else do xs' <- mxs
                     lookup (NE.fromList ts) xs'
