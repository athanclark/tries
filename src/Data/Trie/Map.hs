{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , TupleSections
  , TypeFamilies
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , RankNTypes
  , ScopedTypeVariables
  #-}

module Data.Trie.Map where

import Data.Trie.Class (Trie (..))

import Prelude hiding (lookup, null)
import qualified Data.Map           as Map
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import qualified Data.Key           as K
import qualified Data.Foldable      as F
import Data.Maybe (fromMaybe, fromJust)
import Data.Semigroup ()
import Data.Monoid (First (..), Last (..))
import Control.Monad (replicateM)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..), resize, choose, sized, scale)
import Test.QuickCheck.Instances ()

-- * One Step

data MapChildren c p a = MapChildren
  { mapNode     :: !(Maybe a)
  , mapChildren :: !(Maybe (c p a))
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance ( NFData (c p a)
         , NFData p
         , NFData a
         ) => NFData (MapChildren c p a)

instance ( Arbitrary a
         , Arbitrary p
         , Arbitrary (c p a)
         ) => Arbitrary (MapChildren c p a) where
  arbitrary = MapChildren <$> arbitrary <*> scale (`div` 2) arbitrary

instance (Semigroup (c p a)) => Semigroup (MapChildren c p a) where
  (MapChildren mx mxs) <> (MapChildren my mys) = MapChildren (getLast $ Last mx <> Last my) (mxs <> mys)

instance (Monoid (c p a)) => Monoid (MapChildren c p a) where
  mempty = MapChildren Nothing Nothing

newtype MapStep c p a = MapStep
  { unMapStep :: Map.Map p (MapChildren c p a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance ( NFData (c p a)
         , NFData p
         , NFData a
         ) => NFData (MapStep c p a)

instance ( Arbitrary a
         , Arbitrary p
         , Arbitrary (c p a)
         , Ord p
         ) => Arbitrary (MapStep c p a) where
  arbitrary = sized go
    where
      go n = do
        i <- choose (0,n)
        xs <- replicateM i $ (,) <$> arbitrary <*> resize (n `div` 2) arbitrary
        return $ MapStep $ Map.fromList xs


-- | No insertion instance - requires children nodes to be a monoid. Use @Data.Trie.Map.insert@
-- instead.
instance (Ord p, Trie NonEmpty p c) => Trie NonEmpty p (MapStep c) where
  lookup (p:|ps) (MapStep xs)
    | F.null ps = mapNode =<< Map.lookup p xs
    | otherwise = lookup (NE.fromList ps) =<< mapChildren =<< Map.lookup p xs
  delete (p:|ps) (MapStep xs)
    | F.null ps = let mxs = mapChildren =<< Map.lookup p xs
                  in  MapStep (Map.insert p (MapChildren Nothing mxs) xs)
    | otherwise = let (MapChildren mx mxs) = fromMaybe (MapChildren Nothing Nothing) (Map.lookup p xs)
                  in  MapStep (Map.insert p (MapChildren mx (delete (NE.fromList ps) <$> mxs)) xs)

insert :: ( Ord p
          , Trie NonEmpty p c
          , Monoid (c p a)
          ) => NonEmpty p -> a -> MapStep c p a -> MapStep c p a
insert (p:|ps) x (MapStep xs)
  | F.null ps = let mxs = mapChildren =<< Map.lookup p xs
                in  MapStep (Map.insert p (MapChildren (Just x) mxs) xs)
  | otherwise = let mx  = mapNode =<< Map.lookup p xs
                    xs' = fromMaybe mempty (mapChildren =<< Map.lookup p xs)
                in  MapStep (Map.insert p (MapChildren mx (Just (Data.Trie.Class.insert (NE.fromList ps) x xs'))) xs)

{-# INLINEABLE insert #-}


instance (Ord s, Semigroup (c s a)) => Semigroup (MapStep c s a) where
  (MapStep xs) <> (MapStep ys) = MapStep $ Map.unionWith (<>) xs ys

instance (Ord s, Monoid (c s a)) => Monoid (MapStep c s a) where
  mempty = empty

empty :: MapStep c s a
empty = MapStep Map.empty

singleton :: s -> a -> MapStep c s a
singleton p x = MapStep (Map.singleton p (MapChildren (Just x) Nothing))


-- * Fixpoint of Steps

newtype MapTrie s a = MapTrie
  { unMapTrie :: MapStep MapTrie s a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Semigroup, Monoid, Arbitrary)

instance Ord s => Trie NonEmpty s MapTrie where
  lookup ts (MapTrie xs)   = lookup ts xs
  delete ts (MapTrie xs)   = MapTrie (delete ts xs)
  insert ts x (MapTrie xs) = MapTrie (Data.Trie.Map.insert ts x xs)

type instance K.Key (MapTrie s) = NonEmpty s

-- instance K.Keyed (MapTrie t) where
  -- mapWithKey

instance Ord s => K.Lookup (MapTrie s) where
  lookup = lookup

-- * Conversion

keys :: forall s a. Ord s => MapTrie s a -> [NonEmpty s]
keys (MapTrie (MapStep xs)) =
  F.concatMap go (Map.keys xs)
  where
    go :: s -> [NonEmpty s]
    go k = let (MapChildren _ mxs) = fromJust (Map.lookup k xs)
           in  case mxs of
                 Nothing -> []
                 Just xs' -> NE.cons k <$> keys xs'

elems :: MapTrie s a -> [a]
elems = F.toList

-- * Query

subtrie :: Ord s => NonEmpty s -> MapTrie s a -> Maybe (MapTrie s a)
subtrie (p:|ps) (MapTrie (MapStep xs))
  | F.null ps = mapChildren =<< Map.lookup p xs
  | otherwise = subtrie (NE.fromList ps) =<< mapChildren =<< Map.lookup p xs

-- lookupNearest ~ match
match :: Ord s => NonEmpty s -> MapTrie s a -> Maybe (NonEmpty s, a, [s])
match (p:|ps) (MapTrie (MapStep xs)) = do
  (MapChildren mx mxs) <- Map.lookup p xs
  let mFoundHere = do x <- mx
                      pure (p:|[], x, ps)
  if F.null ps
  then mFoundHere
  else getFirst $
         First (do  (pre,y,suff) <- match (NE.fromList ps) =<< mxs
                    pure (p:|NE.toList pre, y, suff))
      <> First mFoundHere

-- | Returns a list of all the nodes along the path to the furthest point in the
-- query, in order of the path walked from the root to the furthest point.
matches :: Ord s => NonEmpty s -> MapTrie s a -> [(NonEmpty s, a, [s])]
matches (p:|ps) (MapTrie (MapStep xs)) =
  let (MapChildren mx mxs) =
          fromMaybe (MapChildren Nothing Nothing) $
            Map.lookup p xs
      foundHere = fromMaybe [] $ do x <- mx
                                    return [(p:|[],x,ps)]
  in  if F.null ps
      then foundHere
      else let rs = fromMaybe [] $ matches (NE.fromList ps) <$> mxs
           in foundHere ++ (prependAncestry <$> rs)
  where prependAncestry (pre,x,suff) = (p:| NE.toList pre,x,suff)


--
-- lookupVia :: Ord t => (t -> Map.Map t (Maybe x, Maybe (MapTrie t x)) -> Maybe (t, (Maybe x, Maybe (MapTrie t x))))
--                    -> NonEmpty t -> MapTrie t x -> Maybe (NonEmpty t, x)
-- lookupVia f (t:|ts) (MapTrie xs) = case ts of
--   [] -> do (_,(mx,_)) <- f t xs
--            x <- mx
--            return (t:|[],x)
--   _  -> do (_,(_,mxs)) <- Map.lookupLT t xs
--            lookupVia f (NE.fromList ts) =<< mxs
--
