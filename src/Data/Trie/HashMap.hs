{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , TypeFamilies
  , TupleSections
  #-}

module Data.Trie.HashMap where

import Data.Trie.Class (Trie (..))
import Data.Monoid (First (..), Last (..), (<>))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Foldable      as F
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Lazy  as HM
import qualified Data.Key           as K
import Control.Monad (replicateM)

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Prelude hiding (lookup, null)
import Test.QuickCheck (Arbitrary (..), resize, choose, sized, scale)
import Test.QuickCheck.Instances ()


-- * One Step

data HashMapChildren c p a = HashMapChildren
  { hashMapNode     :: !(Maybe a)
  , hashMapChildren :: !(Maybe (c p a))
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance ( NFData (c p a)
         , NFData p
         , NFData a
         ) => NFData (HashMapChildren c p a)

instance ( Arbitrary a
         , Arbitrary p
         , Arbitrary (c p a)
         ) => Arbitrary (HashMapChildren c p a) where
  arbitrary = HashMapChildren <$> arbitrary <*> scale (`div` 2) arbitrary

instance ( Monoid (c p a)
         ) => Monoid (HashMapChildren c p a) where
  mempty = HashMapChildren Nothing Nothing
  mappend (HashMapChildren mx mxs) (HashMapChildren my mys) =
    HashMapChildren (getLast (Last mx <> Last my))
                    (mxs <> mys)

newtype HashMapStep c p a = HashMapStep
  { unHashMapStep :: HM.HashMap p (HashMapChildren c p a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

instance ( NFData (c p a)
         , NFData p
         , NFData a
         ) => NFData (HashMapStep c p a)

instance ( Arbitrary a
         , Arbitrary p
         , Arbitrary (c p a)
         , Hashable p
         , Eq p
         ) => Arbitrary (HashMapStep c p a) where
  arbitrary = sized go
    where
      go n = do
        i <- choose (0,n)
        xs <- replicateM i $ (,) <$> arbitrary <*> resize (n `div` 2) arbitrary
        pure (HashMapStep (HM.fromList xs))

instance ( Hashable p
         , Eq p
         , Trie NonEmpty p c
         ) => Trie NonEmpty p (HashMapStep c) where
  lookup (p:|ps) (HashMapStep xs)
    | F.null ps = hashMapNode
                =<< HM.lookup p xs
    | otherwise = lookup (NE.fromList ps)
                =<< hashMapChildren
                =<< HM.lookup p xs
  delete (p:|ps) (HashMapStep xs)
    | F.null ps = let mxs = hashMapChildren =<< HM.lookup p xs
                  in  HashMapStep (HM.insert p (HashMapChildren Nothing mxs) xs)
    | otherwise = let (HashMapChildren mx mxs) =
                        fromMaybe (HashMapChildren Nothing Nothing)
                                  (HM.lookup p xs)
                  in  HashMapStep (HM.insert p
                                    (HashMapChildren mx (delete (NE.fromList ps) <$> mxs))
                                    xs)

insert :: ( Hashable p
          , Eq p
          , Trie NonEmpty p c
          , Monoid (c p a)
          ) => NonEmpty p -> a -> HashMapStep c p a -> HashMapStep c p a
insert (p:|ps) x (HashMapStep xs)
  | F.null ps = let mxs = hashMapChildren =<< HM.lookup p xs
                in  HashMapStep (HM.insert p
                                  (HashMapChildren (Just x) $! mxs)
                                  xs)
  | otherwise = let mx  = hashMapNode =<< HM.lookup p xs
                    xs' = fromMaybe mempty (hashMapChildren =<< HM.lookup p xs)
                in  HashMapStep (HM.insert p
                                  (HashMapChildren mx
                                    (Just (Data.Trie.Class.insert (NE.fromList ps) x xs')))
                                  xs)

{-# INLINEABLE insert #-}

instance ( Hashable p
         , Eq p
         , Monoid (c p a)
         ) => Monoid (HashMapStep c p a) where
  mempty = empty
  mappend (HashMapStep xs) (HashMapStep ys) =
    HashMapStep (HM.unionWith (<>) xs ys)

empty :: HashMapStep c p a
empty = HashMapStep HM.empty

singleton :: Hashable p => p -> a -> HashMapStep c p a
singleton p x = HashMapStep (HM.singleton p (HashMapChildren (Just x) Nothing))

{-# INLINEABLE singleton #-}

-- * Fixpoint of Steps


newtype HashMapTrie p a = HashMapTrie
  { unHashMapTrie :: HashMapStep HashMapTrie p a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, Arbitrary)


instance ( Hashable p
         , Eq p
         ) => Trie NonEmpty p HashMapTrie where
  lookup ts (HashMapTrie xs)   = lookup ts xs
  delete ts (HashMapTrie xs)   = HashMapTrie (delete ts xs)
  insert ts x (HashMapTrie xs) = HashMapTrie (Data.Trie.HashMap.insert ts x xs)

type instance K.Key (HashMapTrie p) = NonEmpty p

instance ( Hashable p
         , Eq p
         ) => K.Lookup (HashMapTrie p) where
  lookup = lookup

-- * Conversion

keys :: ( Hashable p
        , Eq p
        ) => HashMapTrie p a -> [NonEmpty p]
keys (HashMapTrie (HashMapStep xs)) =
  let ks = HM.keys xs
  in  F.concatMap go ks
  where
    go k = let (HashMapChildren _ mxs) = fromJust (HM.lookup k xs)
           in  case mxs of
                 Nothing -> []
                 Just xs' -> NE.cons k <$> keys xs'

{-# INLINEABLE keys #-}

elems :: HashMapTrie p a -> [a]
elems = F.toList

-- * Query


subtrie :: ( Hashable p
           , Eq p
           ) => NonEmpty p -> HashMapTrie p a -> Maybe (HashMapTrie p a)
subtrie (p:|ps) (HashMapTrie (HashMapStep xs))
  | F.null ps = hashMapChildren =<< HM.lookup p xs
  | otherwise = subtrie (NE.fromList ps) =<< hashMapChildren =<< HM.lookup p xs

{-# INLINEABLE subtrie #-}

-- lookupNearest ~ match
match :: ( Hashable p
         , Eq p
         ) => NonEmpty p -> HashMapTrie p a -> Maybe (NonEmpty p, a, [p])
match (p:|ps) (HashMapTrie (HashMapStep xs)) = do
  (HashMapChildren mx mxs) <- HM.lookup p xs
  let mFoundHere = (p:|[],,ps) <$> mx
  if F.null ps
  then mFoundHere
  else getFirst $ First (do (pre,y,suff) <- match (NE.fromList ps) =<< mxs
                            pure (NE.cons p pre, y, suff))
               <> First mFoundHere

{-# INLINEABLE match #-}

-- | Returns a list of all the nodes along the path to the furthest point in the
-- query, in order of the path walked from the root to the furthest point.
matches :: ( Hashable p
           , Eq p
           ) => NonEmpty p -> HashMapTrie p a -> [(NonEmpty p, a, [p])]
matches (p:|ps) (HashMapTrie (HashMapStep xs)) =
  let (HashMapChildren mx mxs) = fromMaybe mempty (HM.lookup p xs)
      foundHere = case mx of
        Nothing -> []
        Just x -> [(p:|[],x,ps)]
  in  if F.null ps
      then foundHere
      else  let rs = case mxs of
                  Nothing -> []
                  Just xs' -> matches (NE.fromList ps) xs'
            in  foundHere ++ (prependAncestry <$> rs)
  where prependAncestry (pre,x,suff) = (NE.cons p pre,x,suff)

{-# INLINEABLE matches #-}


