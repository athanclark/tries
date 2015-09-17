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


-- * One Step

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


instance (Ord s, Monoid (c s a)) => Monoid (MapStep c s a) where
  mempty = empty
  mappend (MapStep xs) (MapStep ys) = MapStep $ Map.unionWith go xs ys
    where go (mx,mxs) (my,mys) = (getLast $ Last mx <> Last my, mxs <> mys)

empty :: MapStep c s a
empty = MapStep Map.empty

singleton :: s -> a -> MapStep c s a
singleton p x = MapStep $ Map.singleton p (Just x, Nothing)


-- * Fixpoint of Steps

newtype MapTrie s a = MapTrie
  { unMapTrie :: MapStep MapTrie s a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Monoid, Trie NonEmpty s)

type instance K.Key (MapTrie s) = NonEmpty s

-- instance K.Keyed (MapTrie t) where
  -- mapWithKey

instance Ord s => K.Lookup (MapTrie s) where
  lookup = lookup

-- * Conversion

keys :: Ord s => MapTrie s a -> [NonEmpty s]
keys (MapTrie (MapStep xs)) = let ks = Map.keys xs
                              in F.concatMap go ks
  where go k = let (_,mxs) = fromJust $ Map.lookup k xs
               in fmap (k :|) $ fromMaybe [] $ do xs' <- mxs
                                                  return $ NE.toList <$> keys xs'

elems :: MapTrie s a -> [a]
elems = F.toList

-- * Query

subtrie :: Ord s => NonEmpty s -> MapTrie s a -> Maybe (MapTrie s a)
subtrie (p:|ps) (MapTrie (MapStep xs))
  | F.null ps = do (_,mxs) <- Map.lookup p xs
                   mxs
  | otherwise = do (_,mxs) <- Map.lookup p xs
                   subtrie (NE.fromList ps) =<< mxs

-- lookupNearest ~ match
match :: Ord s => NonEmpty s -> MapTrie s a -> Maybe (NonEmpty s, a, [s])
match (p:|ps) (MapTrie (MapStep xs))
  | F.null ps = do (mx,_) <- Map.lookup p xs
                   x <- mx
                   return (p:|[], x, [])
  | otherwise = do (_,mxs) <- Map.lookup p xs
                   (p',y,ps') <- match (NE.fromList ps) =<< mxs
                   return (p:| NE.toList p', y, ps')

-- lookupThrough ~ matches
matches :: Ord s => NonEmpty s -> MapTrie s a -> [(NonEmpty s, a, [s])]
matches (p:|ps) (MapTrie (MapStep xs))
  | F.null ps = F.toList $ do (mx,_) <- Map.lookup p xs
                              x <- mx
                              return (p:|[], x, [])
  | otherwise = let (mx,mxs) = fromMaybe (Nothing,Nothing) $ Map.lookup p xs
                    mrs = matches (NE.fromList ps) <$> mxs
                    x = fromMaybe [] $ ((:[]) . (p:|[],,ps)) <$> mx
                    rs = fromMaybe [] mrs
                in x ++ (prepend1of3 <$> rs)
  where prepend1of3 (a,b,c) = (p:| NE.toList a,b,c)


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
