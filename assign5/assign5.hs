--Author: Litong Peng

import Parser4
import Scanner
import MPoly
import Data.List
import Data.Char

--1.
--(a)
data RegExp sigma = RegEmpty                             |
                    RegEpsilon                           |
                    RegSym sigma                         |
                    RegOr (RegExp sigma) (RegExp sigma)  |
                    RegSeq (RegExp sigma) (RegExp sigma) |
                    RegStar (RegExp sigma)               |
                    RegAnd (RegExp sigma) (RegExp sigma) |
                    RegNot (RegExp sigma)              deriving (Eq,Show)

--(b)
nullable :: (RegExp sigma) -> Bool
nullable RegEmpty     = False
nullable RegEpsilon   = True
nullable (RegSym  u)  = False
nullable (RegOr u v)  = (nullable u) || (nullable v)
nullable (RegSeq u v) = (nullable u) && (nullable v)
nullable (RegStar u)  = True
nullable (RegAnd u v) = (nullable u) && (nullable v)
nullable (RegNot u)   = not (nullable u)

--(c)
nu :: (RegExp sigma) -> (RegExp sigma)
nu x | (nullable x == True) = RegEpsilon
     | otherwise            = RegEmpty

--(d)
deriv :: (Eq a) => a -> (RegExp a) -> (RegExp a)
deriv x RegEmpty               = RegEmpty
deriv x RegEpsilon             = RegEmpty
deriv x (RegSym u) | (x == u)  = RegEpsilon
                   | otherwise = RegEmpty
deriv x (RegOr u v)            = RegOr (deriv x u) (deriv x v)
deriv x (RegSeq u v)           = RegOr (RegSeq (deriv x u) v) (RegSeq (nu u) (deriv x v))
deriv x (RegStar u)            = RegSeq (deriv x u) (RegStar u)
deriv x (RegAnd u v)           = RegAnd (deriv x u) (deriv x v)
deriv x (RegNot u)             = RegNot (deriv x u)

--(e)
match :: (Eq a) => [a] -> (RegExp a) -> Bool
match [] x     = nullable x
match (u:us) x = match us (deriv u x)

--Graduate Problems
remv :: EqnSeq -> [Eqn]
remv (Seq a) = a

result :: [Char] -> [Integer] -> [Char]
result [] []         = ""
result (x:xs) (y:ys) = [x] ++ "=" ++ (show y) ++ " " ++ (result xs ys)

letter :: [Char] -> [Char] -> [Char]
letter [] n                                            = n
letter (x:xs) n | ((isAlpha x) && (elem x n == False)) = letter xs (n ++ [x]) 
                | otherwise                            = letter xs n

solution :: IO [Integer]
solution = 
  do putStr "System: "
     string <- getLine
     if string == "done"
     then return []
     else do putStrLn (result (letter (show string) []) (tointeger (solveSystem (todoublelist (system (remv (parse (tokenStreamFromString string))))))))
             solution

grad :: IO ()
grad = 
  do putStrLn "Enter a system of equations."
     putStrLn "When finished, type ’done’."
     s <- solution
     putStr ""
-- System: x + y = 5, y - x = 1
-- x=2 y=3 
-- System: x + y = z + 1, z = 2*y - 2, z + 3*x = 10
-- x=2 y=3 z=4 
-- System: done


--assign3.3.
--(a)
sub :: (Num a, Eq a) => [a] -> [a] -> [a]
sub [] []         = []
sub (x:xs) (y:ys) = x-y:(sub xs ys)

--(b)
scaleList :: (Num a) => a -> [a] ->[a]
scaleList x []     = []
scaleList x (y:ys) = x*y:(scaleList x ys)

--(c)
subScale :: (Num a, Eq a, Fractional a) => [a] -> [a] -> [a]
subScale (x:xs) (y:ys) = sub ys (scaleList (y/x) xs)

--(d)
nonZeroFirst :: (Num a, Eq a) => [[a]] -> [[a]]
nonZeroFirst lst=
  let helper [] e | e == lst        = error "all the lists start with a zero"
                  | otherwise       = e
      helper (x:xs) e | head x == 0 = helper xs (e ++ [x])
                      | head x /= 0 = [x] ++ e ++ xs
  in helper lst []

--(e)
triangulate ::  [[Double]] -> [[Double]]
triangulate lst = concat [[head (iterate gaussian lst !! s)] | s <- [0..length lst-1]]

gaussian ::  [[Double]] -> [[Double]]
gaussian (x:xs) = nonZeroFirst (concat [[subScale x s] | s <- xs])

--(f)
dot :: (Num a) => [a] -> [a] -> a
dot lst1 lst2 = sum (zipWith (*) lst1 lst2)

--(g)
solveLine :: (Num a, Fractional a) => [a] -> [a] -> a
solveLine lst1 lst2 = ((last lst1) - (dot (tail (init lst1)) lst2)) / (head lst1)

--(h)
solveTriangular :: (Eq a, Fractional a) => [[a]] -> [a]
solveTriangular lst =
  let helper _ [] =[]
      helper s (x:xs) = (solveLine x (helper s xs)):(helper s xs)
  in helper [] lst

remain :: Eq a => [[a]] -> [[a]] -> [[a]]
remain [] ys     = ys
remain (x:xs) ys = remain xs (remove x ys)

remove :: Eq a => [a] -> [[a]] -> [[a]]
remove element list = filter (\e -> e /= element) list

--(i)
solveSystem :: [[Double]] -> [Double]
solveSystem = solveTriangular . triangulate

--assign3.5.
toMPoly :: Exp -> MPoly
toMPoly (RExp n)   = Const n
toMPoly (Var x)    = fromVar x
toMPoly (Sum u v)  = (toMPoly u) + (toMPoly v)
toMPoly (Prod u v) = (toMPoly u) * (toMPoly v)
toMPoly (Diff u v) = (toMPoly u) - (toMPoly v)
toMPoly (Quo u v) = (toMPoly u) / (toMPoly v)

val :: [(String, Rational)] -> String-> Rational                        
val env c = case lookup c env of
              Nothing -> toRational (0)
              Just n  -> n

convertAll :: [Eqn] -> [[(String, Rational)]]
convertAll []     = []
convertAll (x:xs) = (convert x):(convertAll xs)
  where convert (Eqn x y) = findAll (toMPoly x) (toMPoly y) 
 
findAll :: MPoly -> MPoly -> [(String, Rational)]
findAll (ProdPlus (Const a) (KVar b) (Const c)) (Const d) = [(b, a)] ++ [("val", (d - c))]
findAll (ProdPlus (Const e) (KVar b) (Const f)) (ProdPlus (Const g) (KVar d) (Const h))
                                           | b == d    = [(b, (e - g))] ++ [("val", f - h)]
                                           | otherwise = [(b, e)] ++ [(d, -g)] ++ [("val", (h - f))]
findAll (ProdPlus (Const a) (KVar b) c) d                 = [(b, a)] ++ (findAll c d) 

remDup :: [[(String, Rational)]]->[String] 
remDup []     = []
remDup (x:xs) = sortBy compare (delete "val" (nub ((helper x) ++ (remDup xs))))
  where helper []          = []
        helper ((x, _):xs) = x:(helper xs)
		
nonZeroFirst1 :: [[Rational]] -> [[Rational]]
nonZeroFirst1 lst = if (find1 lst) == [] then lst else (find1 lst):(delete (find1 lst) lst)

find1 :: [[Rational]] -> [Rational]
find1 ((x:xs):lst) = if x /= (toRational 0) then (x:xs) else (if lst /= [] then (find1 lst) else [])
			
system1 ::  [[(String, Rational)]] -> [String] -> [[Rational]]
system1 (x:[]) y = [(system2 x y)]
system1 (x:xs) y = [(system2 x y)] ++ (system1 xs y)

system2 :: [(String, Rational)] -> [String] -> [Rational]
system2 y []     = []
system2 y (x:xs) = take (length (x:xs) + 1) (((val y x):(system2 y xs)) ++ [(val y "val")])

system :: [Eqn] -> [[Rational]]
system xs = nonZeroFirst1 (system1 y x)
  where x = remDup (convertAll xs)
        y = convertAll xs

todoublelist :: [[Rational]] -> [[Double]]
todoublelist []     = []
todoublelist (l:ls) = [todouble l] ++ todoublelist ls

todouble :: [Rational] -> [Double]
todouble []     = []
todouble (c:cs) = (fromRational c):(todouble cs)

instance (Integral Double)  where
   toInteger a = round a
   quotRem a b = (fromIntegral(a / b), a - (b * (fromIntegral(a / b))))
   div a b     = a/b

tointeger :: [Double] -> [Integer]
tointeger []     = []
tointeger (c:cs) = (toInteger c):(tointeger cs)

instance Fractional MPoly where
  (Const x) / (Const y) = Const (x / y)
  fromRational = fromConst . fromRational 

