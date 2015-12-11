{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , FlexibleInstances
  , ScopedTypeVariables
  #-}

module Data.Trie.Pseudo where

import Prelude hiding (foldl, foldr, foldr1, lookup, map)
import           Data.Foldable             hiding (all)
import           Data.List.NonEmpty        (NonEmpty (..), fromList)
import qualified Data.List.NonEmpty        as NE

import           Control.Arrow             (second)


-- TODO: difference
-- | Tagged rose tree with explicit emptyness
data PseudoTrie t a = More t (Maybe a) (NonEmpty (PseudoTrie t a))
                    | Rest (NonEmpty t) a
                    | Nil
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Overwriting instance
instance (Eq t) => Monoid (PseudoTrie t a) where
  mempty = Nil
  mappend = merge

beginsWith :: (Eq t) => PseudoTrie t a -> t -> Bool
beginsWith Nil _ = False
beginsWith (Rest (t:|_) _) p = t == p
beginsWith (More t _ _) p    = t == p

-- | Provides a form of deletion by setting a path to @Nothing@, but doesn't
-- cleanup like @prune@
assign :: (Eq t) => NonEmpty t -> Maybe a -> PseudoTrie t a -> PseudoTrie t a
assign ts (Just x) Nil = Rest ts x
assign _  Nothing  Nil = Nil
assign tss@(t:|ts) mx ys@(Rest pss@(p:|ps) y)
  | tss == pss = case mx of
                   (Just x) -> Rest pss x
                   Nothing  -> Nil
  | t == p = case (ts,ps) of
               ([],_) -> More t mx $ Rest (NE.fromList ps) y :| []
               (_,[]) -> case mx of
                           Just x  -> More p (Just y) $ Rest (NE.fromList ts) x :| []
                           Nothing -> ys
               (t':_,p':_) -> if t' == p'
                                then More t Nothing $
                                       assign (NE.fromList ts) mx (Rest (NE.fromList ps) y) :| []
                                else case mx of -- disjoint
                                       Nothing  -> ys
                                       Just x   -> More t Nothing $ NE.fromList
                                                     [ Rest (NE.fromList ps) y
                                                     , Rest (NE.fromList ts) x
                                                     ]
  | otherwise = ys
assign (t:|ts) mx y@(More p my ys)
  | t == p = case ts of
               [] -> More p mx ys
               _  -> More p my $ fmap (assign (NE.fromList ts) mx) ys
  | otherwise = y

-- | Overwrite the LHS point-wise with the RHS's contents
merge :: (Eq t) => PseudoTrie t a -> PseudoTrie t a -> PseudoTrie t a
merge Nil y = y
merge x Nil = x
merge xx@(Rest tss@(t:|ts) x) (Rest pss@(p:|ps) y)
  | tss == pss = Rest pss y
  | t == p = case (ts,ps) of
               ([],_) -> More t (Just x) $ Rest (NE.fromList ps) y :| []
               (_,[]) -> More t (Just y) $ Rest (NE.fromList ts) x :| []
               (_,_)  -> More t Nothing $
                           merge (Rest (NE.fromList ts) x)
                                 (Rest (NE.fromList ps) y) :| []
  | otherwise = xx
merge xx@(More t _ xs) (More p my ys)
  | t == p = More p my $ NE.fromList $
               foldr go [] $ NE.toList xs ++ NE.toList ys
  | otherwise = xx
  where
    go q [] = [q]
    go q (z:zs) | areDisjoint q z = q : z : zs
                | otherwise = merge q z : zs
merge xx@(More t mx xs) (Rest (p:|ps) y)
  | t == p = case ps of
               [] -> More t (Just y) xs
               _  -> More t mx $
                       fmap (flip merge $ Rest (NE.fromList ps) y) xs
  | otherwise = xx
merge xx@(Rest (t:|ts) x) (More p my ys)
  | t == p = case ts of
               [] -> More p (Just x) ys
               _  -> More p my $
                       fmap (merge $ Rest (NE.fromList ts) x) ys
  | otherwise = xx


add :: (Eq t) => NonEmpty t -> PseudoTrie t a -> PseudoTrie t a -> PseudoTrie t a
add ts input container =
  let ts' = NE.toList ts in
  merge container $ mkMores ts' input
  where
    mkMores :: (Eq t) => [t] -> PseudoTrie t a -> PseudoTrie t a
    mkMores [] trie = trie
    mkMores (p:ps) trie = More p Nothing $
      mkMores ps trie :| []


toAssocs :: PseudoTrie t a -> [(NonEmpty t, a)]
toAssocs = go [] []
  where
    go :: [t] -> [(NonEmpty t, a)] -> PseudoTrie t a -> [(NonEmpty t, a)]
    go _     acc Nil = acc
    go depth acc (Rest ts x) = (NE.fromList $ depth ++ NE.toList ts, x) : acc
    go depth acc (More t Nothing xs) =
      foldr (flip $ go $ depth ++ [t]) acc $ NE.toList xs
    go depth acc (More t (Just x) xs) =
      (NE.fromList $ depth ++ [t], x) :
        (foldr $ flip $ go $ depth ++ [t]) acc (NE.toList xs)

fromAssocs :: (Eq t) => [(NonEmpty t, a)] -> PseudoTrie t a
fromAssocs = foldr (uncurry assign) Nil . fmap (second Just)

lookup :: (Eq t) => NonEmpty t -> PseudoTrie t a -> Maybe a
lookup _   Nil = Nothing
lookup tss (Rest pss a)
  | tss == pss = Just a
  | otherwise = Nothing
lookup (t:|ts) (More p mx xs)
  | t == p = case ts of
               [] -> mx
               (t':_) -> find (hasNextTag t') xs >>= lookup (fromList ts)
  | otherwise = Nothing

  where
    hasNextTag :: (Eq t) => t -> PseudoTrie t a -> Bool
    hasNextTag _ Nil = False
    hasNextTag tag (More tag' _ _)    = tag == tag'
    hasNextTag tag (Rest (tag':|_) _) = tag == tag'

-- | Simple test on the heads of two tries
areDisjoint :: (Eq t) => PseudoTrie t a -> PseudoTrie t a -> Bool
areDisjoint (More t _ _) (More p _ _)
  | t == p = False
  | otherwise = True
areDisjoint (Rest (t:|_) _) (Rest (p:|_) _)
  | t == p = False
  | otherwise = True
areDisjoint _ _ = True

-- | The meet of two @PseudoTrie@s
intersectionWith :: (Eq t) =>
                    (a -> b -> c)
                 -> PseudoTrie t a
                 -> PseudoTrie t b
                 -> PseudoTrie t c
intersectionWith _ _ Nil = Nil
intersectionWith _ Nil _ = Nil
intersectionWith f (Rest tss x) (Rest pss y)
  | tss == pss = Rest pss $ f x y
  | otherwise = Nil
intersectionWith f (More t mx xs) (More p my ys)
  | t == p = case [intersectionWith f x' y' | x' <- NE.toList xs, y' <- NE.toList ys] of
               [] -> case f <$> mx <*> my of
                       Nothing -> Nil
                       Just c  -> Rest (p :| []) c
               zs -> More p (f <$> mx <*> my) $ NE.fromList zs
  -- implicit root
  | otherwise = Nil
intersectionWith f (More t mx xs) (Rest (p:|ps) y)
  | t == p = case ps of
               [] -> case f <$> mx <*> Just y of
                     Nothing -> Nil
                     Just c  -> Rest (p :| []) c
               _  -> More p Nothing $ fmap (flip (intersectionWith f) $ Rest (fromList ps) y) xs
  | otherwise = Nil
intersectionWith f (Rest (t:|ts) x) (More p my ys)
  | t == p = case ts of
               [] -> case f <$> Just x <*> my of
                     Nothing -> Nil
                     Just c  -> Rest (t :| []) c
               _  -> More t Nothing $ fmap (intersectionWith f $ Rest (fromList ts) x) ys
  | otherwise = Nil

-- difference :: Eq t =>
--               PseudoTrie t a
--            -> PseudoTrie t a
--            -> PseudoTrie t a


-- | Needless intermediary elements are turned into shortcuts, @Nil@'s in
-- subtrees are also removed.
prune :: PseudoTrie t a -> PseudoTrie t a
prune = go
  where
    go Nil = Nil
    go xx@(Rest _ _) = xx
    go (More t Nothing xs) =
      case cleaned xs of
        [Nil]       -> Nil
        [Rest ts x] -> Rest (t:|NE.toList ts) x
        xs'         -> More t Nothing $ NE.fromList xs'
    go (More t (Just x) xs) =
      case cleaned xs of
        [Nil] -> Rest (t:|[]) x
        xs'   -> More t (Just x) $ NE.fromList xs'

    cleaned xs = removeNils (NE.toList $ fmap go xs)

    removeNils xs = case removeNils' xs of
                      [] -> [Nil]
                      ys -> ys
      where
        removeNils' []       =     []
        removeNils' (Nil:xs') =     removeNils' xs'
        removeNils' (x:xs')   = x : removeNils' xs'
