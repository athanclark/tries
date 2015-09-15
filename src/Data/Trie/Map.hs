{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , GeneralizedNewtypeDeriving
  , TupleSections
  #-}

module Data.Trie.Map where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Maybe
import Data.Monoid


newtype MapTrie t x = MapTrie
  { unMapTrie :: Map.Map t (Maybe x, Maybe (MapTrie t x)) }
  deriving (Show,Eq,Ord,Functor,Foldable)

-- * Query

null :: MapTrie t x -> Bool
null = Map.null . unMapTrie

size :: MapTrie t x -> Int
size = getSum . foldMap (Sum . const 1)

member :: Ord t => NonEmpty t -> MapTrie t x -> Bool
member (t:|ts) (MapTrie xs) = case Map.lookup t xs of
  Just (mx,mxs) -> case ts of
    [] -> isJust mx
    _  -> fromMaybe False $ member (NE.fromList ts) <$> mxs
  Nothing -> False

notMember :: Ord t => NonEmpty t -> MapTrie t x -> Bool
notMember ts = not . member ts

lookup :: Ord t => NonEmpty t -> MapTrie t x -> Maybe x
lookup (t:|ts) (MapTrie xs) = do
  (mx,mxs) <- Map.lookup t xs
  case ts of
    [] -> mx
    _  -> do xs' <- mxs
             lookup (NE.fromList ts) xs'

lookupWithDefault :: Ord t => x -> NonEmpty t -> MapTrie t x -> x
lookupWithDefault x ts = fromMaybe x . lookup ts

lookupVia :: Ord t => (t -> Map.Map t (Maybe x, Maybe (MapTrie t x)) -> Maybe (t, (Maybe x, Maybe (MapTrie t x))))
                   -> NonEmpty t -> MapTrie t x -> Maybe (NonEmpty t, x)
lookupVia f (t:|ts) (MapTrie xs) = case ts of
  [] -> do (_,(mx,_)) <- f t xs
           x <- mx
           return (t:|[],x)
  _  -> do (_,(_,mxs)) <- Map.lookupLT t xs
           lookupVia f (NE.fromList ts) =<< mxs



-- * Construction

empty :: MapTrie t x
empty = MapTrie Map.empty

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


-- lookupThrough :: Ord t => NonEmpty t -> MapTrie t x -> [x]
-- lookupNearest?
