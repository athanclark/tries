module Data.Trie.Map where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE


newtype MapTrie t x = MapTrie
  { unMapTrie :: Map.Map t (Maybe x, Maybe (MapTrie t x)) }

-- * Query

-- * Construction

singleton :: NonEmpty t -> x -> MapTrie t x
singleton (t:|ts) x = case ts of
  [] -> MapTrie $ Map.singleton t (Just x, Nothing)
  _ -> MapTrie $ Map.singleton t (Nothing, Just $ singleton (NE.fromList ts) x)


insert ::Ord t => NonEmpty t -> x -> MapTrie t x -> MapTrie t x
insert (t:|ts) x (MapTrie xs) = case Map.lookup t xs of
  Just (mx, mxs) -> case ts of
    [] -> MapTrie $ Map.insert t (Just x, mxs) xs
    _  -> MapTrie $ Map.insert t (mx, insert (NE.fromList ts) x <$> mxs) xs
  Nothing -> MapTrie $ Map.insert t (Just x, Nothing) xs

update :: Ord t => (x -> Maybe x) -> NonEmpty t -> MapTrie t x -> MapTrie t x
update f (t:|ts) (MapTrie xs) = case ts of
  [] -> MapTrie $ Map.update (\(mx',mxs') -> Just (f =<< mx', mxs')) t xs
  _  -> MapTrie $ Map.update (\(mx',mxs') -> Just (mx',update f (NE.fromList ts) <$> mxs')) t xs

lookup :: Ord t => NonEmpty t -> MapTrie t x -> Maybe x
lookup (t:|ts) (MapTrie xs) = do
  (mx,mxs) <- Map.lookup t xs
  if null ts then mx
             else do xs' <- mxs
                     lookup (NE.fromList ts) xs'

-- lookupThrough :: Ord t => NonEmpty t -> MapTrie t x -> [x]
-- lookupNearest?
