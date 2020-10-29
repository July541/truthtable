module TruthTable.TableExp where

import qualified Data.Set as S

-- | Variables defination
data Var = A | B | C | D | E | F deriving (Eq, Ord)

-- | Basic logic computations
data TableExp = TAnd TableExp TableExp |
                TOr TableExp TableExp  |
                TNot TableExp          |
                TImp TableExp TableExp |
                TVar Var
                deriving (Eq)

instance Show Var where
    show A = "a"
    show B = "b"
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"

instance Show TableExp where
    show (TVar x) = show x
    show (TNot x) = "¬" ++ show x
    show (TAnd a b) = "(" ++ show a ++ "∧" ++ show b ++ ")"
    show (TOr a b) = "(" ++ show a ++ "∨" ++ show b ++ ")"
    show (TImp a b) = "(" ++ show a ++ "→" ++ show b ++ ")"


-- | Compute the length of a table expression for presentation
tableLength :: TableExp -> Int
tableLength (TVar _) = 1
tableLength (TNot x) = 1 + tableLength x
tableLength (TAnd a b) = 1 + tableLength a + tableLength b
tableLength (TOr a b) = 1 + tableLength a + tableLength b
tableLength (TImp a b) = 1 + tableLength a + tableLength b

-- | Extract Vars from a table expression
tableExpVars :: TableExp -> S.Set Var
tableExpVars (TVar x) = S.singleton x
tableExpVars (TNot x) = tableExpVars x
tableExpVars (TAnd a b) = S.union (tableExpVars a) (tableExpVars b)
tableExpVars (TOr a b) = S.union (tableExpVars a) (tableExpVars b)
tableExpVars (TImp a b) = S.union (tableExpVars a) (tableExpVars b)
