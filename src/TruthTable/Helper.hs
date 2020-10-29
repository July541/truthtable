module TruthTable.Helper where

bool2Int :: Bool -> Int
bool2Int False = 0
bool2Int True = 1

bool2IntStr :: Bool -> String
bool2IntStr = show . bool2Int

-- | Generate all candidates for every variable
genBoolVals :: Int -> [[Bool]]
genBoolVals 0 = []
genBoolVals 1 = [[False], [True]]
genBoolVals n = fmap (False:) others ++ fmap (True:) others
    where
        others = genBoolVals (n - 1)
