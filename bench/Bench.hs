module Main where

import qualified Data.List.NonEmpty as NE
import Data.Trie.List as L
import Criterion.Main


listTrie = ListTrie $
  Node (1, Just 1)
    [ Node (2, Just 2)
      [ Node (3, Just 3)
        [ Node (4, Just 4)
          [ Node (5, Just 5) []
          ]
        ]
      ]
    ]



main = defaultMain
  [ bgroup "lookup"
    [ bgroup "ListTrie"
      [ bench "1" $ whnf (L.lookup $ NE.fromList [1])         listTrie
      , bench "2" $ whnf (L.lookup $ NE.fromList [1,2])       listTrie
      , bench "3" $ whnf (L.lookup $ NE.fromList [1,2,3])     listTrie
      , bench "4" $ whnf (L.lookup $ NE.fromList [1,2,3,4])   listTrie
      , bench "4" $ whnf (L.lookup $ NE.fromList [1,2,3,4,5]) listTrie
      ]
    ]
  ]
