module Assign3 where

--Arthor: Litong Peng (lp5629)

import Series
import MPoly
import Sequences
import Data.Ratio
import Data.List

--1.
sinPowSeD :: PowSe Double
sinPowSeD = integratePlus cosPowSeD 0

cosPowSeD :: PowSe Double
cosPowSeD = integratePlus (negate sinPowSeD) 1

sinPowSeR :: PowSe Double
sinPowSeR = integratePlus cosPowSeR 0

cosPowSeR :: PowSe Double
cosPowSeR = integratePlus (negate sinPowSeR) 1

--2.
mySine :: (Ord a, Num a, Fractional a) => a -> a
mySine x | (abs x) < 0.000000001 = x
         | otherwise             = 3*(mySine (x/3)) - 4*(mySine (x/3))^3

--3.
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
triangulate :: (Eq a, Fractional a) => [[a]] -> [[a]]
triangulate lst = concat [[head (iterate gaussian lst !! s)] | s <- [0..length lst-1]]

gaussian :: (Eq a, Fractional a) => [[a]] -> [[a]]
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
  let helper lst backward e | (backward == lst) = e
                            | otherwise         = helper lst ([last (remain backward lst)] ++ backward) (e ++ [solveLine (last (remain backward lst)) e])
  in helper lst [last lst] [(last (last lst)) / (head (last lst))] 

remain :: Eq a => [[a]] -> [[a]] -> [[a]]
remain [] ys     = ys
remain (x:xs) ys = remain xs (remove x ys)

remove :: Eq a => [a] -> [[a]] -> [[a]]
remove element list = filter (\e -> e /= element) list

--(i)
solveSystem :: (Eq a, Fractional a) =>[[a]] -> [a]
solveSystem =solveTriangular . triangulate

--4.
--6.(a)
data Exp = RExp Rational  |
           Var String     |
           Sum Exp Exp    |
           Prod Exp Exp   |
           Pow Exp Exp    |
           D Exp String   |
           Ap Op Exp      |
           Int Exp Exp  deriving Eq

instance Show Exp where
  show (RExp n)       = show n
  show (Var x)        = x
  show (Sum u v)      = showSumContext (Sum u v)
  show (Prod u v)     = showSumContext (Prod u v)
  show (Pow u v)      = showSumContext (Pow u v)
  show (D u v)        = "D(" ++ (show u) ++ "," ++ v ++ ")"
  show (Ap u v)       = show u ++ show v
  show (Int u v)      = show u ++ "d" ++ show v

instance Fractional Exp where
  recip x             =  1 / x
  (RExp x) / (RExp y) =  RExp (x * (recip  y))
  fromRational        = error "fromRational not supported for type Exp"

instance Num Exp where
  negate (RExp x)     = negate (RExp x) 
  (RExp x) + (RExp y) = RExp (x + y)
  (RExp x) * (RExp y) = RExp (x * y)
  fromInteger         = error "fromInteger not supported for type Exp"
  abs                 = error "abs not supported for type Exp"
  signum              = error "signum not supported for type Exp"


prodRationalToString :: Rational -> String 
prodRationalToString x | denominator x == 1 = show (numerator x)
                       | otherwise          = (show (numerator x)) ++ "/" ++ (show (denominator x))

rationalToString :: Rational -> String 
rationalToString x | denominator x == 1 = show (numerator x)
                   | otherwise          = "(" ++ (show (numerator x)) ++ "/" ++ (show (denominator x)) ++ ")"

addParens :: String -> String
addParens str = "(" ++ str ++ ")"

showSumContext :: Exp -> String
showSumContext (RExp x)   = rationalToString x
showSumContext (Var x)    = x
showSumContext (Sum u v)  = (showSumContext u) ++ "+" ++ (showSumContext v)
showSumContext (Prod u v) = (showProdContext u) ++ "*" ++ (showProdContext v)
showSumContext (Pow u v)  = (showPowContextLeft u) ++ "^" ++ (showPowContextRight v)
showSumContext (D u v)    = "D(" ++ (show u) ++ "," ++ v ++ ")"
showSumContext (Ap u v)   = show u ++ show v

showProdContext :: Exp -> String
showProdContext (RExp x) | x < 0    = addParens (prodRationalToString x)
                         |otherwise = prodRationalToString x
showProdContext (Var x)             = x
showProdContext (Sum u v)           = addParens ( showSumContext (Sum u v))
showProdContext (Prod u v)          = (showProdContext u) ++ "*" ++ (showProdContext v)
showProdContext (Pow u v)           = (showPowContextLeft u) ++ "^" ++ (showPowContextRight v)
showProdContext (D u v)             = "D(" ++ (show u) ++ "," ++ v ++ ")"
showProdContext (Ap u v)            = show u ++ show v

showPowContextLeft :: Exp -> String
showPowContextLeft (RExp x) | x < 0    = addParens (rationalToString x)
                            |otherwise = rationalToString x
showPowContextLeft (Var x)             = x
showPowContextLeft (Sum u v)           = addParens ( showSumContext (Sum u v))
showPowContextLeft (Prod u v)          = addParens ( showProdContext (Prod u v))
showPowContextLeft (Pow u v)           = addParens ( (showPowContextLeft u) ++ "^" ++ (showPowContextRight v))
showPowContextLeft (D u v)             = "D(" ++ (show u) ++ "," ++ v ++ ")"
showPowContextLeft (Ap u v)            = show u ++ show v

showPowContextRight :: Exp -> String
showPowContextRight (RExp x) | x < 0    = addParens (rationalToString x)
                             |otherwise = rationalToString x
showPowContextRight (Var x)             = x
showPowContextRight (Sum u v)           = addParens ( showSumContext (Sum u v))
showPowContextRight (Prod u v)          = addParens ( showProdContext (Prod u v))
showPowContextRight (Pow u v)           = (showPowContextLeft u) ++ "^" ++ (showPowContextRight v)
showPowContextRight (D u v)             = "D(" ++ (show u) ++ "," ++ v ++ ")"
showPowContextRight (Ap u v)            = show u ++ show v

--5.
data Eqn = Eqn Exp Exp deriving (Eq,Show)

expToMPoly :: Exp -> MPoly
expToMPoly (RExp n)   = Const n
expToMPoly (Var x)    = ProdPlus (Const 1) (KVar x) (Const 0)
expToMPoly (Sum u v)  = expToMPoly u + expToMPoly v
expToMPoly (Prod u v) = expToMPoly u * expToMPoly v

eqnToMPoly :: [Eqn] -> [[MPoly]]
eqnToMPoly [] = []
eqnToMPoly ((Eqn u v):xs) = ([expToMPoly u] ++ [expToMPoly v]):(eqnToMPoly xs)

ordered :: [[MPoly]] -> [[MPoly]]
ordered lst = sortBy compareK lst

compareK :: [MPoly] -> [MPoly] -> Ordering
compareK lst1 lst2 = compareK1 (head lst1) (head lst2)

compareK1 :: MPoly -> MPoly -> Ordering
compareK1 (Const a) (Const b)               = EQ
compareK1 (Const a) (ProdPlus _ _ _)        = GT
compareK1 (ProdPlus _ _ _) (Const a)        = LT
compareK1 (ProdPlus x y z) (ProdPlus u v w) = compare (minM (ProdPlus x y z)) (minM (ProdPlus u v w))

minM :: MPoly -> Kernel
minM (ProdPlus u v (Const w)) = v
minM (ProdPlus u v (ProdPlus x y z)) | v <= y    = minM (ProdPlus u v z)
                                     | otherwise = minM (ProdPlus x y z)

system :: [Eqn] -> [[Rational]]
system []  = []
system lst = system3 (system2 (ordered (eqnToMPoly lst)))
  
system3 :: [[Rational]] -> [[Rational]]
system3 lst= 
  let helper [] a = []
      helper (x:xs) a | (length x) == a = x:(helper xs a)
                      | otherwise       = (foldr (\c r->[c]++r) x (replicate (a-(length x)) 0)):(helper xs a)
  in helper lst (maxLength lst)

maxLength :: [[Rational]] -> Int
maxLength (x:y) | length x >= length y = length x
                | otherwise            = length y
maxLength (x:y:ys) | length x >= length y = maxLength (x:ys)
                   | otherwise            = maxLength (y:ys)

system2 :: [[MPoly]] -> [[Rational]]
system2 []     = []
system2 (x:xs) = ((system1 (head x)) ++ (system1 (last x))):(system2 xs) 

system1 :: MPoly -> [Rational]
system1 (Const x) | x==0      = []
                  | otherwise = [x]
system1 (ProdPlus (Const m) y (Const n)) | (m == 0 && n /= 0) = [n]
                                         | (m /= 0 && n == 0) = [m]
                                         | otherwise          = [m,n] 
system1 (ProdPlus (Const m) y (ProdPlus o p q)) | m == 0               = system1 (ProdPlus o p q)
                                                | (m /= 0) && (y <= p) = [m] ++ system1 (ProdPlus o p q)
                                                | (m /= 0) && (y>p)    = system1 (ProdPlus o p q) ++ [m]
system1 (ProdPlus (ProdPlus o p q) y z) = error "non_linear"

--6.
--(b)
deriv3 :: Exp -> Exp
deriv3 (D (RExp n) x) = RExp 0
deriv3 (D (Var x) y) 
  | x == y    = RExp 1
  | otherwise = RExp 0
deriv3 (D (Sum u v) x) = Sum (deriv3 (D u x)) (deriv3 (D v x))
deriv3 (D (Prod u v) x) = Sum (Prod u (deriv3 (D v x))) (Prod v (deriv3 (D u x)))
deriv3 (D (Pow u (RExp n)) x) = (Prod (Prod (RExp n) (Pow u (RExp (n-1)))) (deriv3 (D u x)))
deriv3 (D (Pow (RExp n) v) x) = (D (Pow (RExp n) v) x)
deriv3 (Prod u v) = Prod u (deriv3 v)
deriv3 (Int (Prod u v) x) = (Prod (RExp (1/2)) (Prod u u))

visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(RExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))
visitOnce f (D u v)    = f (D (visitOnce f u) v)
visitOnce f (Ap u v)   = f (Ap u v)

visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new

simp3 :: Exp -> Exp
simp3 (Sum (RExp n) (RExp m))           = RExp (n+m)
simp3 (Sum (RExp n) v)                  = (Sum v (RExp n))
simp3 (Sum u (RExp 0))                  = u
simp3 (Sum (Sum u (RExp n)) (RExp m))   = Sum u (RExp (n+m))
simp3 (Sum (Sum u (RExp n)) v)          = Sum (Sum u v) (RExp n)
simp3 (Sum u (Sum v w))                 = Sum (Sum u v) w
simp3 (Sum u v)
  | u == v                              = Prod (RExp 2) u
simp3 (Sum (Prod (RExp n) u) v)
  | u == v                              = Prod (RExp (n+1)) u
simp3 (Sum u (Prod (RExp n) v))
  | u == v                              = Prod (RExp (n+1)) u
simp3 (Sum (Prod (RExp n) u) (Prod (RExp m) v))
  | u == v                              = Prod (RExp (n+m)) u
simp3 (Prod (RExp n) (RExp m))          = RExp (n*m)
simp3 (Prod u (RExp n))                 = Prod (RExp n) u
simp3 (Prod (RExp 0) v)                 = RExp 0
simp3 (Prod (RExp 1) v)                 = v
simp3 (Prod (RExp n) (Prod (RExp m) v)) = Prod (RExp (n*m)) v
simp3 (Prod u (Prod (RExp n) v))        = Prod (RExp n) (Prod u v)
simp3 (Prod (Prod u v) w)               = Prod u (Prod v w)
simp3 (Prod (RExp n) (Sum u v))         = Sum (Prod (RExp n) u) (Prod (RExp n) v)
simp3 (Pow u (RExp 0))                  = RExp 1
simp3 (Pow u (RExp 1))                  = u
simp3 u                                 = u
simp3 (Prod (RExp a) (Prod u v))
  | u == v                              = (RExp (1/2)) * u * u

simplify3 :: Exp -> Exp
simplify3 e = visitUntilUnchanged simp3 (deriv3 e)

--Graduate Problems
--1.
--(a)
derivN :: Fractional a => (a -> a) -> a -> a
derivN f n = ((f (n + 0.000000001)) - (f n)) / 0.000000001

--(b)
data DualNum a = DualNum a a deriving (Eq, Show)

instance (Num a, Fractional a) => Num (DualNum a) where
  negate (DualNum x y)          = DualNum (negate x) (negate y)
  (DualNum x y) + (DualNum m n) = DualNum (x + m) (y + n)
  (DualNum x y) * (DualNum m n) = DualNum (x * m) (x * n + m * y)
  fromInteger n                 = DualNum (fromInteger n) 0
  abs _                         = error "abs not supported for type DualNum"
  signum _                      = error "signum not supported for type DualNum"

instance (Num a, Fractional a) => Fractional (DualNum a) where
  recip (DualNum x y) = DualNum (1 / x) ((negate y) / y * y)
  x / y               = x * recip y
  fromRational c      = DualNum (fromRational c) 0

deri :: DualNum a -> a
deri (DualNum x y) = y

d :: (Num a, Fractional a) => ((DualNum a) -> (DualNum a)) -> a -> a
d f x = deri (f (DualNum x 1))

newtonImprove :: (Num a, Fractional a) => ((DualNum a) -> (DualNum a)) -> a -> a
newtonImprove f x = x - a / b where DualNum a b = f (DualNum x 1.0)

improveCurtGuess :: Fractional a => a -> a -> a
improveCurtGuess a g = ((a/g^2+2*g)/3)

curtApprox :: Fractional a => a -> Seq a
curtApprox a = Seq (iterate (improveCurtGuess a) 1)

curt :: Double -> Double
curt = limitD . curtApprox

--2.
--(a)
data Op = Sin | Cos | Exp | Ln deriving (Eq, Show)

-- *Assign3> simplify3 (Int (Prod (Var "y") (D (Var "y") "x")) (Var "x"))
-- 1/2*y*y
-- *Assign3> simplify3 (Int (Prod (Ap Sin (Var "x")) (Ap Cos (Var "x"))) (Var "x"))
-- 1/2*Sinx*Sinx

--(b)






