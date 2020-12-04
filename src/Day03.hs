module Day03 where

import Data.List

day03a :: String -> Int
day03a str =  calculateTrees $ treePosition (lines str)

-- Elem indices is 0 based index, converting it to 1 based. Because it's easier to
-- calculate modulous.
treePosition :: [String] -> [[Int]]
treePosition inputLines = map (\input -> map (+1) $ elemIndices '#' input) inputLines

-- Checks if the tree is present at the position
-- Return 1 : If yes else 0
-- Since position of tree repeats, we can use modulo to see if a tree is present at a position
-- 31 is the number of columuns present in the input file.
checkTree :: Int -> [Int] -> Int
checkTree pos input = 
  if (pos `mod` 31 == 0) || ((pos `mod` 31 ) `elem` input )
    then 1
    else 0

calculateTrees :: [[Int]] -> Int
calculateTrees inputs =
  length $ filter (==1) $ map (\x -> checkTree (fst x) (snd x)) inputListWithPosition
  -- Associate the position we will be at for each input, since our offset is 3 (Please remeber that I have assumed 1 based indexing)
  -- On second line, we will at 4th position, on 3rd line we will be at 7th position as so on
  -- i.e the coordinate for the car is (1,1) (2,4) (3,7) (4,10)
  where inputListWithPosition = zip [4,7..] (tail inputs)

