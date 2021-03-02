module SudokuConfig where


--author: Litong Peng (lp5629)


import Solver
import Data.Char
import Data.List


--1.4.
data SudokuConfig = SudokuConfig [Int] deriving Eq


--2.

sudokuConfigFromList :: Integral a => [a] -> SudokuConfig
sudokuConfigFromList nums = SudokuConfig (map fromIntegral nums)


--3.
listFromSudokuConfig :: SudokuConfig -> [Int]
listFromSudokuConfig (SudokuConfig nums) = nums


--5.
split :: Int -> [a] -> [[a]]
split p [] = []
split p xs = y:(split p ys)
  where (y, ys) = splitAt p xs

instance Show SudokuConfig where
  show (SudokuConfig nums) = concat ("\n":[ row ++ "\n" | row <- rows ])
    where
      ns = [ (number n):" " | n <- nums ]
      number n | n == 0 = '_'
               | otherwise = intToDigit n
      rows = [ splitLine line | line <- split 9 ns ]
      splitLine l = concat [ (concat three) ++ "  " | three <- split 3 l ]
        

--6.
nextNum :: SudokuConfig -> (Maybe Int)
nextNum (SudokuConfig nums) = elemIndex 0 nums

badRow ::SudokuConfig -> Int -> [Int]
badRow (SudokuConfig nums) x = take 9 (drop ((div x 9) * 9) nums)

badColumn :: SudokuConfig -> Int -> [Int]
badColumn (SudokuConfig nums) x = (columns (SudokuConfig nums)) !! (rem x 9)

badBlock :: SudokuConfig -> Int -> [Int]
badBlock (SudokuConfig nums) x | (elem x [0,1,2,9,10,11,18,19,20])     = (take 3 nums) ++ (take 3 (drop 9 nums)) ++ (take 3 (drop 18 nums))
                               | (elem x [3,4,5,12,13,14,21,22,23])    = (take 3 (drop 3 nums)) ++ (take 3 (drop 12 nums)) ++ (take 3 (drop 21 nums))
                               | (elem x [6,7,8,15,16,17,24,25,26])    = (take 3 (drop 6 nums)) ++ (take 3 (drop 15 nums)) ++ (take 3 (drop 24 nums))
                               | (elem x [27,28,29,36,37,38,45,46,47]) = (take 3 (drop 27 nums)) ++ (take 3 (drop 36 nums)) ++ (take 3 (drop 45 nums))
                               | (elem x [30,31,32,39,40,41,48,49,50]) = (take 3 (drop 30 nums)) ++ (take 3 (drop 39 nums)) ++ (take 3 (drop 48 nums))
                               | (elem x [33,34,35,42,43,44,51,52,53]) = (take 3 (drop 33 nums)) ++ (take 3 (drop 42 nums)) ++ (take 3 (drop 51 nums))
                               | (elem x [54,55,56,63,64,65,72,73,73]) = (take 3 (drop 54 nums)) ++ (take 3 (drop 63 nums)) ++ (take 3 (drop 72 nums))
                               | (elem x [57,58,59,66,67,68,75,76,77]) = (take 3 (drop 57 nums)) ++ (take 3 (drop 66 nums)) ++ (take 3 (drop 75 nums))
                               | (elem x [60,61,62,69,70,71,78,79,80]) = (take 3 (drop 60 nums)) ++ (take 3 (drop 69 nums)) ++ (take 3 (drop 78 nums))

changedToOptions :: SudokuConfig -> Int -> [Int]
changedToOptions (SudokuConfig nums) empty = [1..9] \\ canNot
  where
    badR    = badRow (SudokuConfig nums) empty
    badC    = badColumn (SudokuConfig nums) empty
    badB    = badBlock (SudokuConfig nums) empty
    canNot  = badR ++ badC ++ badB


satisfy :: [Int] -> Bool
satisfy xs = sort xs == [1..9]

rows :: SudokuConfig -> [[Int]]
rows (SudokuConfig nums) = split 9 nums

columns :: SudokuConfig -> [[Int]]
columns (SudokuConfig nums)= transpose (rows (SudokuConfig nums))

blocks :: SudokuConfig -> [[Int]]
blocks (SudokuConfig nums)= map concat (concatMap (split 3) (transpose (map (split 3) (rows (SudokuConfig nums)))))


instance Config SudokuConfig where
  successors (SudokuConfig nums) =
    case nextNum (SudokuConfig nums) of
      Nothing    -> []
      Just empty -> new empty
    where
      new empty =[SudokuConfig (before ++ (changedTo:(tail after)))| changedTo <- changedToOptions (SudokuConfig nums) empty]
        where (before, after) = splitAt empty nums
  isGoal (SudokuConfig nums) = isFull && rowsSatisfy && columnsSatisfy && blocksSatisfy
    where
      isFull         = nextNum (SudokuConfig nums) == Nothing
      rowsSatisfy    = and (map satisfy (rows (SudokuConfig nums)))
      columnsSatisfy = and (map satisfy (columns (SudokuConfig nums)))
      blocksSatisfy  = and (map satisfy (blocks (SudokuConfig nums)))


--7.
sudokuSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudokuSolve = solve isGoal