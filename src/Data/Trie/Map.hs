{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , TupleSections
  , TypeFamilies
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module Data.Trie.Map where

import Data.Trie.Class

import Prelude hiding (lookup, null)
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import qualified Data.Set.Class as S
import qualified Data.Key as K
import qualified Data.Foldable as F
import Data.Maybe
import Data.Monoid


newtype MapStep c p a = MapStep
  { unMapStep :: Map.Map p (Maybe a, Maybe (c p a)) }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


instance (Ord p, Trie NonEmpty p c) => Trie NonEmpty p (MapStep c) where
  lookup (p:|ps) (MapStep xs)
    | F.null ps = do (mx,_) <- Map.lookup p xs
                     mx
    | otherwise = do (_,mxs) <- Map.lookup p xs
                     lookup (NE.fromList ps) =<< mxs
  insert (p:|ps) x (MapStep xs)
    | F.null ps = let mxs = snd =<< Map.lookup p xs
                  in  MapStep $ Map.insert p (Just x,mxs) xs
    | otherwise = let (mx,mxs) = fromMaybe (Nothing,Nothing) $ Map.lookup p xs
                  in  MapStep $ Map.insert p (mx, insert (NE.fromList ps) x <$> mxs) xs
  delete (p:|ps) (MapStep xs)
    | F.null ps = let mxs = snd =<< Map.lookup p xs
                  in  MapStep $ Map.insert p (Nothing,mxs) xs
    | otherwise = let (mx,mxs) = fromMaybe (Nothing,Nothing) $ Map.lookup p xs
                  in  MapStep $ Map.insert p (mx, delete (NE.fromList ps) <$> mxs) xs

-- instance (Ord p, Monoid (c p a), Monoid a) => Monoid (MapStep c p a) where
--   mempty = empty
--   mappend (MapStep xs) (MapStep ys) = MapStep $ Map.unionWith mappend xs ys
--
-- empty :: MapStep c p a
-- empty = MapStep Map.empty
--
-- singleton :: p -> a -> MapStep c p a
-- singleton p x = MapStep $ Map.singleton p (Just x, Nothing)
--
--
-- newtype MapTrie p a = MapTrie
--   { unMapTrie :: MapStep MapTrie p a }
--   deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Monoid, Trie (NonEmpty p))
--
-- type instance K.Key (MapTrie p) = NonEmpty p

-- instance K.Keyed (MapTrie t) where
  -- mapWithKey

-- instance Ord p => K.Lookup (MapTrie p) where
--   lookup = lookup

-- * Query

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
--
--
-- -- * Construction
--
--
-- insert ::Ord t => NonEmpty t -> x -> MapTrie t x -> MapTrie t x
-- insert (t:|ts) x (MapTrie xs) = case Map.lookup t xs of
--   Just (mx, mxs) -> case ts of
--     [] -> MapTrie $ Map.insert t (Just x, mxs) xs
--     _  -> MapTrie $ Map.insert t (mx, insert (NE.fromList ts) x <$> mxs) xs
--   Nothing -> MapTrie $ Map.insert t (Just x, Nothing) xs
--
-- update :: Ord t => (x -> Maybe x) -> NonEmpty t -> MapTrie t x -> MapTrie t x
-- update f (t:|ts) (MapTrie xs) = case ts of
--   [] -> MapTrie $ Map.update (\(mx',mxs') -> Just (f =<< mx', mxs')) t xs
--   _  -> MapTrie $ Map.update (\(mx',mxs') -> Just (mx',update f (NE.fromList ts) <$> mxs')) t xs


-- lookupThrough :: Ord t => NonEmpty t -> MapTrie t x -> [x]
-- lookupNearest?
