module Series where

-- author: Arthur Nunes-Harwitt


import Data.Ratio
import Sequences

newtype  PowSe a = PowSe [a]

signumString :: (Eq a, Num a) => a -> String
signumString n = 
  case (signum n) of 
     (-1) -> "-"
     (0)  -> ""
     (1)  -> "+"

instance (Eq a, Fractional a, Show a) => Show (PowSe a) where
  show (PowSe ser) = 
    let show' _ []     = ("","0")
        show' n [a]    = ((signumString a), "...")
        show' n (a:as) = 
          let (sign, str) = show' (n+1) as
          in if (signumString a) == ""
             then (sign, str)
             else ((signumString a), (showTerm (abs a) n) ++ sign ++ str)
        showTerm 0 _ = ""
        showTerm a 0 = show a
        showTerm a 1 = (show a)++"*x"
        showTerm a n = (show a)++"*x^"++(show n)
        (sign, str) = show' 0 (take 11 ser)
    in case sign of 
         ""  -> "0+"++str
         "-" -> sign++str
         "+" -> str

--PowSe []
--0+0
--PowSe [1]
-- ...
--Series> PowSe [1..]
--1.0+2.0*x+3.0*x^2+4.0*x^3+5.0*x^4+6.0*x^5+7.0*x^6+8.0*x^7+9.0*x^8+10.0*x^9+...

shift :: (Eq a, Fractional a) => PowSe a -> PowSe a
shift (PowSe xs) = PowSe (0:xs)

--shift (PowSe [1..])
--1.0*x+2.0*x^2+3.0*x^3+4.0*x^4+5.0*x^5+6.0*x^6+7.0*x^7+8.0*x^8+9.0*x^9+...

scale :: (Eq a, Fractional a) => a -> PowSe a -> PowSe a
scale c (PowSe xs) = PowSe [c*x | x <- xs]

--scale 5 (shift (PowSe [1..]))
--5.0*x+10.0*x^2+15.0*x^3+20.0*x^4+25.0*x^5+30.0*x^6+35.0*x^7+40.0*x^8+45.0*x^9+...

zeros :: (Eq a, Fractional a) => PowSe a
zeros = let zs = 0:zs in PowSe zs

--zeros
--0+...

one :: (Eq a, Fractional a) => PowSe a
one   = let (PowSe zs) = zeros in PowSe (1:zs)

--one
--1.0...

var :: (Eq a, Fractional a) => PowSe a
var   = shift one

--var
-- 1.0*x...


instance (Eq a, Fractional a) => Num (PowSe a) where
  (PowSe (x:xs)) + (PowSe (y:ys)) = let (PowSe zs) = (PowSe xs) + (PowSe ys)
                                    in PowSe ((x+y):zs)
  negate (PowSe xs) = PowSe [(-x) | x <- xs]
  (PowSe (x:xs)) * (PowSe (y:ys)) =
    let term4      = shift ((PowSe xs) * (PowSe ys))
        term3      = scale y (PowSe xs)
        term2      = scale x (PowSe ys)
        (PowSe zs) = term2 + term3 + term4
    in PowSe ((x*y):zs)
  abs _ = error "abs not supported for type PowSes"
  signum _ = error "signum not supported for type PowSes"
  fromInteger n = let (PowSe zs) = zeros
                  in PowSe ((fromInteger n):zs)

--(PowSe [1..])+(PowSe [1..])
--2.0+4.0*x+6.0*x^2+8.0*x^3+10.0*x^4+12.0*x^5+14.0*x^6+16.0*x^7+18.0*x^8+20.0*x^9+...
--negate (PowSe [1..])
---1.0-2.0*x-3.0*x^2-4.0*x^3-5.0*x^4-6.0*x^5-7.0*x^6-8.0*x^7-9.0*x^8-10.0*x^9-...
--(PowSe [1..])*(PowSe [2..])
--2.0+7.0*x+16.0*x^2+30.0*x^3+50.0*x^4+77.0*x^5+112.0*x^6+156.0*x^7+210.0*x^8+275.0*x^9+...

recipOne :: (Eq a, Fractional a) => PowSe a -> PowSe a  
recipOne (PowSe (1:sr)) =
   let (PowSe recip) = 1 - (shift ((PowSe sr) * (PowSe recip)))
   in (PowSe recip)


instance (Eq a, Fractional a) => Fractional (PowSe a) where
  recip (PowSe (x:xs))
    | x == 0  = error "reciprocal not defined"
    | x == 1  = recipOne (PowSe (x:xs))
    | otherwise = scale (1/x) (recipOne (scale (1/x) (PowSe (x:xs))))
  fromRational q = let (PowSe zs) = zeros
                   in PowSe ((fromRational q):zs)




integratePlus :: (Fractional a, Eq a) => PowSe a -> a -> PowSe a
integratePlus (PowSe xs) c = 
  let scaleRecip (x:xs) n = (x/n):(scaleRecip xs (n+1))
  in PowSe (c:(scaleRecip xs 1))




expPowSeD :: PowSe Double
expPowSeD = integratePlus expPowSeD 1
            

expPowSeR :: PowSe Rational
expPowSeR = integratePlus expPowSeR 1



atanPowSeD :: PowSe Double
atanPowSeD = integratePlus (1/(1+var^2)) 0

atanPowSeR :: PowSe Rational
atanPowSeR = integratePlus (1/(1+var^2)) 0



piPowSeD :: PowSe Double
piPowSeD = let s n = PowSe ((1/n):zs)
                      where
                         (PowSe zs) = (- (s (n+2)))
           in 4*(s 1)


piPowSeR :: PowSe Rational
piPowSeR = let s n = PowSe ((1/n):zs)
                      where
                         (PowSe zs) = (- (s (n+2)))
           in 4*(s 1)



approxFromPowSe :: (Eq a, Fractional a) => Integer -> PowSe a -> a -> a
approxFromPowSe 0 (PowSe (x:xs)) x0 = x
approxFromPowSe n (PowSe (x:xs)) x0 = x + x0*(approxFromPowSe (n-1) (PowSe xs) x0)

seqFromPowSe :: (Eq a, Fractional a) => PowSe a -> a -> Seq a
seqFromPowSe ps x0 = Seq [approxFromPowSe n ps x0 | n <- [0..]]