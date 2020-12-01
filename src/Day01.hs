module Day01 (day01a, day01b) where

import Data.List
import Control.Monad

-- Assume that we will get input in lines
day01a :: String -> Int
day01a str = getNumPair (convertToList str) 2020

-- Convert and sort the list in ascending order
convertToList :: String -> [Int]
convertToList str = sort [ read numStr :: Int | numStr <- lines str]

getNumPair :: [Int] -> Int -> Int
getNumPair l@(x:xs) sum
 | num1 == -1 = getNumPair (init l) sum
 | otherwise = num1 * (last l)
 where num1 = getFirstNum (init l) (last l) sum

getFirstNum :: [Int] -> Int -> Int -> Int
getFirstNum (x:xs) highNum sum 
  | x + highNum == sum = x
  | x + highNum > sum = -1
  | otherwise = getFirstNum xs highNum sum

-- Note: A better solution to the above question would be to higher order functions.
-- The below appraoch is an attempt to use those.

day01b :: String -> Int
day01b str = getTripletsProduct (convertToList str) 2020

checkTripletsSum :: Int -> Int -> Int -> Int -> Int
checkTripletsSum sum a b c 
 | a + b + c == 2020 = a * b * c
 | otherwise = -1

-- Note: The implementation of liftM3 for list is from left to right
-- This basically will help us get the sum of all the number in the list
-- Eg: liftM2 (+) [1,2] [3,4] == [(1+3), (1+4), (2+3), (2+4)]
-- Eg: liftM3 (+) [1,2] [3,4] [5,6] == [(1+3+5), (1+3+6), (1+4+5), (1+4+6), (2+3+5) ...]
getTripletsProduct :: [Int] -> Int -> Int
getTripletsProduct list sum = head . filter (>(-1)) $ liftM3 (checkTripletsSum sum) list list list
