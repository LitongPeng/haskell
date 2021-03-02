module GraduateProblem where


--Author: Litong Peng (lp5629)


import Control.Monad
import Solver



--1.
data BExp = BConst Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp deriving (Eq,Show)


--2.
data SatConfig = SatConfig BExp [(String,Bool)] deriving Eq

instance Show SatConfig where
  show (SatConfig m [n]) = show n                     

instance Config SatConfig where
  sucessors sat@(SatConfig bexp [n]) = suc sat (allVar sat (findVar sat) (allTRuthValue sat (findVar sat)))
          
  isGoal (SatConfig bexp [n]) = (goal (SatConfig bexp [n]))

suc :: SatConfig -> [(String,Bool)] -> [SatConfig]
suc (SatConfig bexp [n]) [x:xs] = (SatConfig bexp [x]):(suc (SatConfig bexp [n]) [xs])

allVar :: SatConfig -> [String] -> [[Bool]] -> [(String,Bool)]
allVar _ xs (y:ys) = zipWith (,) xs y

allTRuthValue :: SatConfig -> [String] -> [[Bool]]
allTRuthValue _ xs = replicaM (length xs) [True,False]

findVar :: SatConfig -> [String]                                                                                 
findVar (SatConfig bexp [n]) =
    case bexp of
      BConst b        -> []
      Var s           -> [n] ++ s
      And bexp1 bexp2 -> (findVar (SatConfig bexp1 [n])) ++ (findVar ( SatConfig bexp2 [n]))
      Or bexp1 bexp2  -> (findVar (SatConfig bexp1 [n])) ++ (findVar (SatConfig bexp2 [n]))
      Not bexp1       -> (findVar (SatConfig bexp1 [n]))
  
isStringTrue :: String -> [(String,Bool)]
isStringTrue s (x:xs) | s == (fst x) = snd x
                      | otherwise = isStringTrue s xs

goal :: SatConfig -> Bool
goal (SatConfig bexp [n]) =
    case bexp of
      BConst b        -> b == True
      Var s           -> isStringTrue (s [n])
      And bexp1 bexp2 -> (goal (SatConfig bexp1 [n]) && goal (SatConfig bexp2 [n])) == True
      Or bexp1 bexp2  -> (goal (SatConfig bexp1 [n]) || goal (SatConfig bexp2 [n])) == True
      Not bexp1       -> (not (goal (SatConfig bexp1 [n]))) == True


--3.
satSolve :: SatConfig -> (Maybe SatConfig)
satSolve = solve isGoal
      

