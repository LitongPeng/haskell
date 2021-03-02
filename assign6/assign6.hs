--author: Litong Peng

--1.
data Const = IConst Integer                 |
             BConst Bool                    |
             FConst1 String (Const -> Const)|
             FConst2 String (Const -> Const -> Const) 

instance Show Const where
  show (IConst x)    = show x
  show (BConst x)    = show x
  show (FConst1 x y) = "<prim1:" ++ x ++">"
  show (FConst2 x y) = "<prim2:" ++ x ++">"

instance Eq Const where 
  (IConst x) == (IConst y)       = x == y
  (BConst x) == (BConst y)       = x == y
  (FConst1 x y) == (FConst1 u v) = x == u
  (FConst2 x y) == (FConst2 u v) = x == u

primAbs :: Const -> Const 
primAbs (IConst x) = (IConst (abs x))

primPlus :: Const -> Const -> Const
primPlus (IConst x) (IConst y) = IConst (x + y)

--2.
data Exp = Econ Const        |
           Var String        |
           Lambda String Exp |
           IfExp Exp Exp Exp |
           Appl Exp Exp      |
           LetRec String String deriving (Show, Eq)

--3.
data O = S   |
         K   |
         I   |
         B   |
         C   |
         CIf |
         Y deriving (Show, Eq)

data CExp = Ccon Const  |
            CVar String |
            Cop O       |
            CAppl CExp CExp deriving (Show, Eq)

--4.
initEnv = [("abs", FConst1 "abs" primAbs),
           ("+", FConst2 "+" primPlus),
           ("-", FConst2 "-" primPlus),
           ("*", FConst2 "*" primPlus),
           ("div", FConst2 "div" primPlus),
           ("==", FConst2 "==" primPlus)]

compile :: Exp -> CExp
compile (Econ x)      = Ccon x
compile (Var x)       = CVar x
compile (IfExp x y z) = (CAppl (CAppl (CAppl (Cop CIf) (compile x)) (compile y)) (compile z))
compile (Lambda x y)  = abstract x (compile y) initEnv
compile (Appl x y)    = CAppl (compile x) (compile y)

dom :: String -> [(String, Const)] -> Bool
dom x [] = False
dom x (y:ys) | x == (fst y) = True
             | otherwise    = dom x ys

pFind :: [(String, Const)] -> String -> Const
pFind [] x = FConst2 x primPlus
pFind (y:ys) x | x == "abs" = FConst1 x primAbs
               | otherwise  = pFind ys x

isKsomething :: CExp -> Bool
isKsomething (CAppl x y) | x == Cop K = True
                         | otherwise  = False
isKsomething (Ccon x) = False
isKsomething (CVar x) = False
isKsomething (Cop x)  = False


abstract :: String -> CExp -> [(String, Const)] -> CExp
abstract x (Ccon c) p = CAppl (Cop K) (Ccon c)
abstract x (CVar y) p | x == y                      = Cop I
abstract x (CVar y) p | (y /= x) && (dom y p)       = CAppl (Cop K) (Ccon (pFind p y))
                      | (y /= x) && (not (dom y p)) = CAppl (Cop K) (CVar y)
abstract x (Cop y) p = CAppl (Cop K) (Cop y)
abstract x (CAppl m n) p | (CAppl (Cop K) m1) <- (abstract x m p), 
                           (Cop I) <- (abstract x n p)                        = m1
                         | (CAppl (Cop K) m1) <- (abstract x m p), 
                           (CAppl (Cop K) n1) <- (abstract x n p)             = (CAppl (Cop K) (CAppl m1 n1))
                         | (CAppl (Cop K) m1) <- (abstract x m p), 
                           n1 <- (abstract x n p),
                           n1 /= (Cop I), isKsomething n1 == False            = (CAppl (CAppl (Cop B) m1) n1)
                         | m1 <- (abstract x m p), 
                           (CAppl (Cop K) n1) <- (abstract x n p),
                           isKsomething m1 == False                           = (CAppl (CAppl (Cop C) m1) n1)
                         | m1 <- (abstract x m p), 
                           n1 <- (abstract x n p),
                           isKsomething m1 == False, isKsomething n1 == False = CAppl (CAppl (Cop S) m1) n1

--5.
reduceComb :: CExp -> CExp
reduceComb (CAppl (Cop I) x)                                           = x
reduceComb (CAppl (CAppl (Cop K) x) y)                                 = x
reduceComb (CAppl (CAppl (CAppl (Cop S) f) g) x)                       = CAppl (CAppl f x) (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop B) f) g) x)                       = CAppl f (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop C) f) x) y)                       = CAppl (CAppl f y) x
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst True))) x) y)  = x
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst False))) x) y) = y
reduceComb (CAppl (Ccon (FConst1 x y)) (Ccon z))                       = Ccon (y z)
reduceComb (CAppl (CAppl (Ccon (FConst2 x y)) (Ccon z)) (Ccon n))      = Ccon (y z n)
reduceComb (CAppl (CAppl (Cop Y) x) y)                                 = (CAppl (CAppl x (Cop Y)) y) 
reduceComb (CAppl x y)                                                 = (CAppl (reduceComb x) (reduceComb y))
reduceComb x                                                           = x

--6.
run :: CExp -> CExp
run = (visitUntilUnchanged reduceComb)

visitUntilUnchanged :: (CExp -> CExp) -> CExp -> CExp
visitUntilUnchanged f x = let y = f x
                           in if y == x
                              then x
                              else visitUntilUnchanged f y

--7.
compileAndRun :: Exp -> CExp
compileAndRun = run . compile




