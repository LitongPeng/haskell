-- Arthor: Litong Peng (lp5629)

import Data.Monoid

--1.
--(a)
index_a :: Eq a => a -> [a] -> Maybe Int
index_a x []                 = Nothing
index_a x (y:ys) | x == y    = Just 0
                 | otherwise = (pure (+)) <*> (Just 1) <*> (index_a x ys)

--(b)
index_b :: Eq a => a -> [a] -> (Maybe Int -> Maybe Int) -> Maybe Int
index_b x [] k                 = k Nothing
index_b x (y:ys) k | x == y    = k (Just 0)
                   | otherwise = index_b x ys (\v -> k((pure (+)) <*> (Just 1) <*> v))

--(c) 
newtype K r a = K ((a -> r) -> r)

(<<<) :: K r a -> (a -> r) -> r
(K f) <<< c = f c

instance Monad (K r) where
  return v = K (\k ->k v)
  m >>= f  = K (\k -> m <<< (\a -> (f a) <<< k))

instance Applicative (K r) where
  pure = return
  mf <*> ma = do f <- mf
                 a <- ma
                 return (f a)

instance Functor (K n) where
  fmap g fx = (pure g) <*> fx

abortWith :: (Maybe Int) -> (K (Maybe Int) (Maybe Int))
abortWith v = K (\k -> v)

index_c ::  Eq a => a -> [a] -> (K (Maybe Int) (Maybe Int))
index_c x []                 = (return Nothing) >>= abortWith
index_c x (y:ys) | x == y    = return (Just 0) 
                 | otherwise = return (index_c x ys <<< (\v -> (+1) <$> v))

topIndex_c :: Eq a => a -> [a] -> Maybe Int
topIndex_c x []     = (index_c x []) <<< id
topIndex_c x (y:ys) = (index_c x (y:ys)) <<< id

--(d)
index_d :: Eq a => a -> [a] -> Maybe Int
index_d x []                 = Nothing
index_d x (y:ys) | x == y    = Just 0
                 | otherwise = (index_d x ys) >>= (\m->Just (m+1))

--(e)
index_e :: Eq a => a -> [a] -> Maybe Int
index_e x []                 = Nothing
index_e x (y:ys) | x == y    = Just 0
                 | otherwise = do m <- (index_e x ys)
                                  return (m+1)

--2.
meetAndGreet :: IO ()
meetAndGreet = 
  do putStr "What is your name? "
     name <- getLine
     putStr "Hello "
     putStr name
     putStrLn "!"

--3.
average :: [Double] -> Double
average lst = sum lst / fromIntegral (length lst)

readDoubles :: IO [Double]
readDoubles =
  do putStr "Enter a number:"
     n <- getLine
     if n == "done"
     then return []
     else do a <- readDoubles; return (((read n)::Double):a)

interface :: IO ()
interface = 
  do putStrLn "Enter some numbers."
     putStrLn "When finished, type 'done'."
     lst <- readDoubles
     putStr "The average is "
     putStrLn (show (average lst))
     putStr "The maximum is "
     putStrLn (show (maximum lst))
     putStr "The minimum is "
     putStrLn (show (minimum lst))


--Graduate Problems
--1.
cp::FilePath ->FilePath->IO ()
cp a b =readFile a >>= writeFile b










