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
  , TypeFamilies
  , TupleSections
  #-}

module Data.Trie.HashMap where

import Data.Trie.Class
import Data.Monoid
import Data.Hashable
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Foldable      as F
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Lazy  as HM
import qualified Data.Key           as K
import Control.Monad

import Data.Data
import GHC.Generics
import Control.DeepSeq
import Prelude hiding (lookup, null)
import Test.QuickCheck
import Test.QuickCheck.Instances ()


-- * One Step

newtype HashMapStep c p a = HashMapStep
  { unHashMapStep :: HM.HashMap p (Maybe a, Maybe (c p a))
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
        xs <- replicateM i $ (,) <$> arbitrary <*> resize (floor (fromIntegral n / 2 :: Float)) arbitrary
        return $ HashMapStep $ HM.fromList xs

instance ( Hashable p
         , Eq p
         , Trie NonEmpty p c
         ) => Trie NonEmpty p (HashMapStep c) where
  lookup (p:|ps) (HashMapStep xs)
    | F.null ps = fst =<< HM.lookup p xs
    | otherwise = lookup (NE.fromList ps) =<< snd =<< HM.lookup p xs
  delete (p:|ps) (HashMapStep xs)
    | F.null ps = let mxs = snd =<< HM.lookup p xs
                  in  HashMapStep $ HM.insert p (Nothing,mxs) xs
    | otherwise = let (mx,mxs) = fromMaybe (Nothing,Nothing) $ HM.lookup p xs
                  in  HashMapStep $ HM.insert p (mx, delete (NE.fromList ps) <$> mxs) xs

insert :: ( Hashable p
          , Eq p
          , Trie NonEmpty p c
          , Monoid (c p a)
          ) => NonEmpty p -> a -> HashMapStep c p a -> HashMapStep c p a
insert (p:|ps) x (HashMapStep xs)
  | F.null ps = let mxs = snd =<< HM.lookup p xs
                in  HashMapStep $ HM.insert p (Just x,mxs) xs
  | otherwise = let mx  = fst =<< HM.lookup p xs
                    xs' = fromMaybe mempty (snd =<< HM.lookup p xs)
                in  HashMapStep $ HM.insert p (mx, Just $ Data.Trie.Class.insert (NE.fromList ps) x xs') xs


instance ( Hashable p
         , Eq p
         , Monoid (c p a)
         ) => Monoid (HashMapStep c p a) where
  mempty = empty
  mappend (HashMapStep xs) (HashMapStep ys) = HashMapStep $ HM.unionWith go xs ys
    where go (mx,mxs) (my,mys) = (getLast $ Last mx <> Last my, mxs <> mys)

empty :: HashMapStep c p a
empty = HashMapStep HM.empty

singleton :: Hashable p => p -> a -> HashMapStep c p a
singleton p x = HashMapStep $ HM.singleton p (Just x, Nothing)

-- * Fixpoint of Steps


newtype HashMapTrie p a = HashMapTrie
  { unHashMapTrie :: HashMapStep HashMapTrie p a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, Arbitrary)


instance ( Hashable p
         , Eq p
         ) => Trie NonEmpty p HashMapTrie where
  lookup ts (HashMapTrie xs) = lookup ts xs
  delete ts (HashMapTrie xs) = HashMapTrie $ delete ts xs
  insert ts x (HashMapTrie xs) = HashMapTrie $ Data.Trie.HashMap.insert ts x xs

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
  in F.concatMap go ks
  where
    go k = let (_,mxs) = fromJust $ HM.lookup k xs
           in fmap (k :|) $ fromMaybe [] $ do xs' <- mxs
                                              return $ NE.toList <$> keys xs'


elems :: HashMapTrie p a -> [a]
elems = F.toList

-- * Query


subtrie :: ( Hashable p
           , Eq p
           ) => NonEmpty p -> HashMapTrie p a -> Maybe (HashMapTrie p a)
subtrie (p:|ps) (HashMapTrie (HashMapStep xs))
  | F.null ps = snd =<< HM.lookup p xs
  | otherwise = subtrie (NE.fromList ps) =<< snd =<< HM.lookup p xs


-- lookupNearest ~ match
match :: ( Hashable p
         , Eq p
         ) => NonEmpty p -> HashMapTrie p a -> Maybe (NonEmpty p, a, [p])
match (p:|ps) (HashMapTrie (HashMapStep xs)) = do
  (mx,mxs) <- HM.lookup p xs
  let mFoundHere = (p:|[],, ps) <$> mx
  if F.null ps
  then mFoundHere
  else getFirst $ First (do (pre,y,suff) <- match (NE.fromList ps) =<< mxs
                            pure (p:|NE.toList pre, y, suff))
               <> First mFoundHere

-- | Returns a list of all the nodes along the path to the furthest point in the
-- query, in order of the path walked from the root to the furthest point.
matches :: ( Hashable p
           , Eq p
           ) => NonEmpty p -> HashMapTrie p a -> [(NonEmpty p, a, [p])]
matches (p:|ps) (HashMapTrie (HashMapStep xs)) =
  let (mx,mxs) = fromMaybe (Nothing,Nothing) $ HM.lookup p xs
      foundHere = fromMaybe [] $ (\x -> [(p:|[],x,ps)]) <$> mx
  in if F.null ps
  then foundHere
  else let rs = fromMaybe [] $ matches (NE.fromList ps) <$> mxs
       in  foundHere ++ (prependAncestry <$> rs)
  where prependAncestry (pre,x,suff) = (p:| NE.toList pre,x,suff)



