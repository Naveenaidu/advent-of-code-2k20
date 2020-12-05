module Day05 where

import Data.Char
import Data.List

-- Since the number-range reduces by half, it hit me that it's equivalent to how
-- Binary number system works.
-- Let's take a look at how the system works.
-- F means `lower half range` and B means `upper half range`
-- i.e If we have a Binary Digit of two number then B will take `1` and F will take `0`
-- Assuming we are workin on two digit binary number
-- `00` -> This Binary represenation is the lowest value in our system (value 0)
-- The range of value above consits of 0,1,2,3

-- If the first digit is 0 then the only possibilits the last digit can have is `0` or `1`
-- Eg: `01` or `00` i.e we have reduced the value range by half (0,1)

-- If the first digit is 1, then the only possibilits the last digit can have is `0` or `1`
-- Eg: `10` or `11`  == (2,3)

-- That means, having 1 in the first digit gives us the `upper half`(2,3) values in our range (0,1,2,3)
-- And having 0 in the first digit gives us the `lower half` (0,1) values in our range (0,1,2,3)

-- Thus looking at our example
-- F means to take the lower half
-- B means to take the upper half
-- Comparing it with our previous example of two digit binary number
-- We get lower half when the first digit is 0 and upper half when first digit is 1
-- Thus we can assume that in our systsem F mean `0` and  B mean `1`
-- This mean: FBFBBFF == 0101100 = 44, FFFBBBF == 0001110 = 14

-- Similary
-- R means to take the upper half
-- L means to take the lower half
-- Thus R == `1` and L == `0`
-- Eg: RRR == 111 = 7, RLL == 100 = 4

day05a :: String -> Int
day05a str = maximum $  map seatId (lines str)

-- Map Letters to Binary
-- F == 0, B==1
convertRowToInt :: String -> Int
convertRowToInt str = foldl (\acc x -> acc * 2 + digitToInt x) 0 rowToBinary
  where rowToBinary = map (\c -> if c == 'F' then '0' else '1') str

-- R == 1, L==0
convertColToInt :: String -> Int
convertColToInt str = foldl (\acc x -> acc * 2 + digitToInt x) 0 colToBinary
  where colToBinary = map (\c -> if c == 'L' then '0' else '1') str
-- Convert Binary String to Integer

seatId :: String -> Int
seatId seatSeq =  row * 8 + col
  where row = convertRowToInt (take 7 seatSeq)
        col = convertColToInt (drop 7 seatSeq)

----------------------------

-- Get the difference between two list
-- Find the min and max seat id
-- Make a list of values in the range [min, max]
-- Get the difference between the input we got and the above list
day05b :: String -> Int
day05b str = getSeatId $ map seatId (lines str)

getSeatId :: [Int] -> Int
getSeatId seats = head $ seatMap \\ seats
  where seatMap = [minimum seats .. maximum seats]
