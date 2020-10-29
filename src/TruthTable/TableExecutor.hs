module TruthTable.TableExecutor (
    truthTable
) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe (fromJust)
import Text.Layout.Table
import           TruthTable.LogicExp
import           TruthTable.TableExp
import           TruthTable.Helper

type TableValues = M.Map Var Bool

-- | set all values for a set of Variables.
-- For example bindValues Set (A, B) -> [Map (A -> 0, B -> 0), Map (A -> 0, B -> 1),
-- Map (A -> 1, B -> 0), Map (A -> 1, B -> 1)]
bindValues :: S.Set Var -> [TableValues]
bindValues st = fmap (M.fromList . zip stLst) $ genBoolVals (length stLst)
    where
        stLst = S.toList st

-- | Convert a table expression to logic expression (with specily values)
applyValue :: TableExp -> TableValues -> Exp
applyValue (TVar x) tv = Id $ fromJust $ M.lookup x tv
applyValue (TAnd a b) tv = And (applyValue a tv) (applyValue b tv)
applyValue (TOr a b) tv = Or (applyValue a tv) (applyValue b tv)
applyValue (TNot x) tv = Not $ applyValue x tv
applyValue (TImp a b) tv = Imp (applyValue a tv) (applyValue b tv)

truthTable' exp = do
    putStrLn $ tableString columns unicodeRoundS (titlesH titles) r
    where
        titles = fmap show vars ++ [show exp]
        vars = S.toList $ tableExpVars exp
        vals = bindValues $ tableExpVars exp
        results = fmap bool2IntStr $ fmap eval $ fmap (applyValue exp) vals
        tmp = zipWith (\x y -> x ++ [y]) (fmap (fmap bool2IntStr) $ (fmap M.elems vals)) results
        r = fmap rowG tmp
        columnLayout len = column (fixed len) center dotAlign def
        columns = (replicate (length titles - 1) (columnLayout 3))  ++ [columnLayout (tableLength exp * 2)]

varWidth :: Int
varWidth = 3

tableExpWidth :: TableExp -> Int
tableExpWidth = (*2) . tableLength

vars :: TableExp -> [Var]
vars = S.toList . tableExpVars

tableTitles :: TableExp -> [String]
tableTitles exp = map show (vars exp) <> [show exp]

defaultColumnLayout :: Int -> ColSpec
defaultColumnLayout fixedSize = column (fixed fixedSize) center dotAlign def

columns :: TableExp -> [ColSpec]
columns exp = (replicate varCnt (defaultColumnLayout varWidth)) <> [defaultColumnLayout $ tableExpWidth exp]
    where
        varCnt = (length $ tableTitles exp) - 1

truthTable :: TableExp -> IO ()
truthTable exp = putStrLn $ tableString (columns exp) unicodeRoundS (titlesH $ tableTitles exp) content
    where
        expCandidates = bindValues $ tableExpVars exp
        results = map (bool2IntStr . eval . (applyValue exp)) expCandidates
        varsStrings = map (map bool2IntStr) $ map M.elems expCandidates
        content = map rowG $ zipWith (\xs s -> xs ++ [s]) varsStrings results