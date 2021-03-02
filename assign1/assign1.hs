--Litong Peng (lp5629)


--1.
second::[Integer]->Integer
second (x:xs)=head (xs)


--2.
singleton::[Integer]->Bool
singleton []    =False
singleton (x:xs)=xs==[]


--3.
catAll::[[a]]->[a]
catAll []    =[]
catAll (x:xs)=x++(catAll xs)


--4.
index::Eq c=> c->[c]->Maybe Integer
index x lst=
   let helper x [] n              =Nothing
       helper x (y:ys) n|x==y     =Just n
                        |otherwise=helper x ys (n+1)
   in helper x lst 0


--5.
evenSquares'::[Integer]->[Integer]
evenSquares' lst=filter even (map (\x->x*x) lst)


--6.
insert::Ord a=> a->[a]->[a]
insert x []              =[x]
insert x (y:ys)|x<y      =x:y:ys
               |otherwise=y:(insert x ys)

insertionSort::Ord a=> [a]->[a]
insertionSort []    =[]
insertionSort (x:xs)=insert x (insertionSort xs)


--7.
insertionSortH::Ord a=>[a]->[a]
insertionSortH (x:xs)=foldr insert [] (x:xs)


--8.
perm::[a]->[[a]]
perm []    =[[]]
perm (x:xs)=foldr (++) [] (map (helper [] x) (perm xs))

helper::[a]->a->[a]->[[a]]
helper xs x []    =[xs++[x]]
helper xs x (y:ys)=(xs++(x:y:ys)):(helper (xs++[y]) x ys)


--9.
--(a)
data Peano=Zero|S Peano deriving Show

--(b)
add::Peano->Peano->Peano
add Zero a =a
add (S a) b=S (add a b)

--(c)
mult::Peano->Peano->Peano
mult Zero b =Zero
mult (S a) b=add b (mult a b)

--(d)
fact::Peano->Peano
fact Zero =(S Zero)
fact (S a)=mult (S a) (fact a)


--Graduate Problem
--1.
--(a)
meaning::Peano->(a->a)->a->a
meaning Zero = \s -> \z -> z
meaning (S n)= \s -> \z -> (s (meaning n s z))

--(b)
fromPeano::Peano->Integer
fromPeano Zero =(meaning Zero (+1) 0)
fromPeano (S n)=(meaning (S n) (+1) 0)
