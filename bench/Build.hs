module Build where



import Data.List.NonEmpty (NonEmpty (..))
import Data.Tree (Tree (..))
import Data.Trie.List              as L
import Data.Trie.Map               as M
import Data.Trie.HashMap           as HM
import Data.Tree.Knuth.Forest      as K
import Data.Trie.Knuth             as K
import qualified Data.Map          as Map
import qualified Data.HashMap.Lazy as HMap
import Control.Monad.State


buildMiddleQuery :: Int -> NonEmpty Int
buildMiddleQuery x = 0 :| go x
  where
    go n =
      let half = floor ((fromIntegral n)/2)
      in if half == 0
         then []
         else half : go (floor $ (fromIntegral n)/2)


genListTrie :: Int -> ListTrie Int Int
genListTrie n = ListTrie $ Node (0, Just 0) $ genTree n
  where
    genTree :: Int -> [Tree (Int, Maybe Int)]
    genTree n = flip evalState 1 $ replicateM n $ do
      i <- getSucc
      return $ Node (i, Just 0) $ genTree (floor $ (fromIntegral n)/2)


getSucc :: State Int Int
getSucc = do
  i <- get
  modify (+1)
  pure i


genMapTrie :: Int -> MapTrie Int Int
genMapTrie n = MapTrie . MapStep . Map.singleton 0 $
  MapChildren (Just 0) $ Just . MapTrie . MapStep $ genMapTree n
  where
    genMapTree :: Int -> Map.Map Int (MapChildren MapTrie Int Int)
    genMapTree n =
      Map.unions . flip evalState 1 $
        replicateM n $ do
          i <- getSucc
          pure $ Map.singleton i $
                   MapChildren (Just 0) $
                     if n <= 1
                     then Nothing
                     else Just . MapTrie . MapStep $ genMapTree (floor $ (fromIntegral n)/2)


genHashMapTrie :: Int -> HashMapTrie Int Int
genHashMapTrie n = HashMapTrie $ HashMapStep $ HMap.singleton 0 $
  HashMapChildren (Just 0) $ Just . HashMapTrie . HashMapStep $ genHashMapTree n
  where
    genHashMapTree :: Int -> HMap.HashMap Int (HashMapChildren HashMapTrie Int Int)
    genHashMapTree n = HMap.unions $ flip evalState 1 $
      replicateM n $ do
        i <- getSucc
        return $ HMap.singleton i $
                   HashMapChildren (Just 0) $
                     if n <= 1
                     then Nothing
                     else Just . HashMapTrie . HashMapStep $ genHashMapTree (floor $ fromIntegral n / 2)



-- | This one is ordered largest first
genKnuthTrie :: Int -> KnuthTrie Int Int
genKnuthTrie n = KnuthTrie $ Fork (0, Just 0) (genKnuthForest n n) Nil
  where
    genKnuthForest :: Int -> Int -> KnuthForest (Int, Maybe Int)
    genKnuthForest 0 _ = Nil
    genKnuthForest n s = let cs = if n == 0 then Nil else genKnuthForest (floor $ (fromIntegral n)/2) (n-1)
                             ss = if s == 0 then Nil else genKnuthForest n (s-1)
                         in Fork (s, Just 0) cs ss
