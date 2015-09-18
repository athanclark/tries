module Main where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Tree (Tree (..))
import Data.Trie.List as L
import Data.Trie.Map as M
import Data.Trie.Class as TC
import qualified Data.Map as Map
import Criterion.Main
import Control.Monad.State


genListTrie :: Int -> ListTrie Int Int
genListTrie n = ListTrie $ Node (0, Just 0) $ genTree n
  where
    genTree :: Int -> [Tree (Int, Maybe Int)]
    genTree n = evalState (replicateM n $ do
      i <- getSucc
      return $ Node (i, Just i) $ genTree (n-1)) 1


getSucc :: State Int Int
getSucc = get <* modify (+1)


genMapTrie :: Int -> MapTrie Int Int
genMapTrie n = MapTrie $ MapStep $ Map.singleton 0
  (Just 0, Just $ MapTrie $ MapStep $ genMapTree n)
  where
    genMapTree :: Int -> Map.Map Int (Maybe Int, Maybe (MapTrie Int Int))
    genMapTree n = Map.unions $ evalState (replicateM n $ do
      i <- getSucc
      return $ Map.singleton i ( Just i
                               , if n <= 1 then Nothing
                                 else Just $ MapTrie $ MapStep $ genMapTree (n-1))) 1



main = defaultMain
  [ bgroup "lookup"
    [ bgroup "ListTrie"
      [ bench "1" $ whnf (L.lookup $ 0 :| [100])         (genListTrie 100)
      , bench "2" $ whnf (L.lookup $ 0 :| [100,99..81])  (genListTrie 100)
      , bench "3" $ whnf (L.lookup $ 0 :| [100,99..61])  (genListTrie 100)
      , bench "4" $ whnf (L.lookup $ 0 :| [100,99..41])  (genListTrie 100)
      , bench "5" $ whnf (L.lookup $ 0 :| [100,99..21])  (genListTrie 100)
      , bench "6" $ whnf (L.lookup $ 0 :| [100,99..1])   (genListTrie 100)
      ]
    , bgroup "MapTrie"
      [ bench "1" $ whnf (TC.lookup $ 0 :| [100])         (genMapTrie 100)
      , bench "2" $ whnf (TC.lookup $ 0 :| [100,99..81])  (genMapTrie 100)
      , bench "3" $ whnf (TC.lookup $ 0 :| [100,99..61])  (genMapTrie 100)
      , bench "4" $ whnf (TC.lookup $ 0 :| [100,99..41])  (genMapTrie 100)
      , bench "5" $ whnf (TC.lookup $ 0 :| [100,99..21])  (genMapTrie 100)
      , bench "6" $ whnf (TC.lookup $ 0 :| [100,99..1])   (genMapTrie 100)
      ]
    ]
  ]
