{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  #-}

module Data.TrieSpec (trieSpec) where

import Prelude hiding (lookup)
import Data.Maybe (isJust, isNothing)
import Data.Trie.Class as TC
import Data.Trie.List  as L
import Data.Tree (Tree (..))
import Data.Trie.Map   as M hiding (insert)
import Data.Trie.Knuth as K hiding (lookup, insert, delete)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances



trieSpec :: [TestTree]
trieSpec =
  [ testGroup "Data.Trie.List"
    [ QC.testProperty "lookup after insertion should exist" lookupInsertExists_List
    , QC.testProperty "lookup after deletion should not exist" lookupDeleteNotExists_List
    ]
  , testGroup "Data.Trie.Map"
    [ QC.testProperty "lookup after insertion should exist" lookupInsertExists_Map
    , QC.testProperty "lookup after deletion should not exist" lookupDeleteNotExists_Map
    ]
  , testGroup "Data.Trie.Knuth"
    [ QC.testProperty "lookup after insertion should exist" lookupInsertExists_Knuth
    , QC.testProperty "lookup after deletion should not exist" lookupDeleteNotExists_Knuth
    ]
  ]



newtype PathWithHead c p a = PathWithHead (NonEmpty p, c p a)
  deriving (Show)

instance (Arbitrary a, Arbitrary p, Eq p) => Arbitrary (PathWithHead ListTrie p a) where
  arbitrary = do ts@(t:|_) <- arbitrary
                 xss <- arbitrary `suchThat` hasHead t
                 return $ PathWithHead (ts,xss)
    where hasHead t (ListTrie (Node (t',_) _)) = t == t'



lookupInsertExists_List :: PathWithHead ListTrie Int Int -> Int -> Bool
lookupInsertExists_List (PathWithHead (ts,trie)) x = lookupInsertExists ts x trie

lookupInsertExists_Map :: NonEmpty Int -> Int -> MapTrie Int Int -> Bool
lookupInsertExists_Map = lookupInsertExists

lookupInsertExists_Knuth :: NonEmpty Int -> Int -> KnuthTrie Int Int -> Bool
lookupInsertExists_Knuth = lookupInsertExists

lookupInsertExists :: ( Arbitrary (p s)
                      , Arbitrary (t s a)
                      , Arbitrary a
                      , Trie p s t
                      ) => p s -> a -> t s a -> Bool
lookupInsertExists ps x trie = isJust $ lookup ps $ insert ps x trie



lookupDeleteNotExists_List :: NonEmpty Int -> ListTrie Int Int -> Bool
lookupDeleteNotExists_List = lookupDeleteNotExists

lookupDeleteNotExists_Map :: NonEmpty Int -> MapTrie Int Int -> Bool
lookupDeleteNotExists_Map = lookupDeleteNotExists

lookupDeleteNotExists_Knuth :: NonEmpty Int -> KnuthTrie Int Int -> Bool
lookupDeleteNotExists_Knuth = lookupDeleteNotExists

lookupDeleteNotExists :: ( Arbitrary (p s)
                         , Arbitrary (t s a)
                         , Arbitrary a
                         , Trie p s t
                         ) => p s -> t s a -> Bool
lookupDeleteNotExists ps trie = isNothing $ lookup ps $ delete ps trie



-- Instances -------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> arbitrary `suchThat` (not . null)
