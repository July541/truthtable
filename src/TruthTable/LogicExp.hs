module TruthTable.LogicExp (
    Exp (..)
  , eval
) where

-- | Logic expression
data Exp = And Exp Exp |
           Or Exp Exp  |
           Not Exp     |
           Imp Exp Exp |
           Id Bool
           deriving (Show)

-- | evaluate logic expression
eval :: Exp -> Bool
eval (Id a) = a
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Not a) = not $ eval a
eval (Imp a b) = not (eval a && not (eval b))
