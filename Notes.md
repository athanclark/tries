Notes
=====


## Generic Size via Foldable

```haskell
size :: Foldable f => f a -> Integer
size = getSum . foldMap (Sum . const 1)
```

## `First` / `Last` for Data.Map

```haskell
instance (Ord k, Monoid a) => Monoid (Map k a) where
  mempty = Map.empty
  mappend = Map.unionWith mappend

newtype FirstVals k a = FirstVals { getFirstVals :: Map k a }

instance (Ord k) => Monoid (FirstVals k a) where
  mempty = FirstVals $ Map.empty
  mappend (FirstVals xs) (FirstVals ys) = FirstVals $ Map.unionWith const xs ys

newtype LastVals k a = LastVals { getLastVals :: Map k a }

instance (Ord k) => Monoid (LastVals k a) where
  mempty = LastVals $ Map.empty
  mappend (LastVals xs) (LastVals ys) = LastVals $ Map.unionWith (const id) xs ys
```
