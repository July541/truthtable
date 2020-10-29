module Main where

import TruthTable

main :: IO ()
main = do
    truthTable (TAnd (TVar A) (TVar B))
    truthTable (TOr (TVar C) (TVar C))
    truthTable (TOr (TVar C) (TVar D))
    truthTable (TAnd (TOr (TVar A) (TVar B)) (TOr (TVar A) (TVar C)))
    truthTable (TNot (TVar A))

