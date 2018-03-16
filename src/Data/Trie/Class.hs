{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Data.Trie.Class where

import Prelude hiding (lookup)
import qualified Data.Trie as BT
import qualified Data.ByteString as BS

import Data.Maybe (isJust)
import Data.Foldable as F
import Data.Functor.Identity (Identity (..))


-- | Class representing tries with single-threaded insertion, deletion, and lookup.
-- @forall ts ps a. isJust $ lookupPath ps (insertPath ps a ts)@
-- @forall ts ps. isNothing $ lookupPath ps (deletePath ps ts)@
class Trie p s t | t -> p where
  lookup :: p s -> t s a -> Maybe a
  insert :: p s -> a -> t s a -> t s a
  delete :: p s -> t s a -> t s a




member :: Trie p s t => p s -> t s a -> Bool
member t = isJust . lookup t

notMember :: Trie p s t => p s -> t s a -> Bool
notMember t = not . member t

-- * Conversion

fromFoldable :: (Foldable f, Monoid (t s a), Trie p s t) => f (p s, a) -> t s a
fromFoldable = F.foldr (uncurry insert) mempty


-- * ByteString-Trie

-- | Embeds an empty ByteString passed around for type inference.
newtype BSTrie q a = BSTrie {unBSTrie :: (q, BT.Trie a)}

makeBSTrie :: BT.Trie a -> BSTrie BS.ByteString a
makeBSTrie x = BSTrie (mempty,x)

getBSTrie :: BSTrie BS.ByteString a -> BT.Trie a
getBSTrie (BSTrie (_,x)) = x

instance Trie Identity BS.ByteString BSTrie where
  lookup (Identity ps) (BSTrie (_,xs)) = BT.lookup ps xs
  insert (Identity ps) x (BSTrie (q,xs)) = BSTrie (q, BT.insert ps x xs)
  delete (Identity ps) (BSTrie (q,xs)) = BSTrie (q, BT.delete ps xs)
