{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text ( Text )
import Data.Char (isSpace)
import Control.Monad (liftM)

import Day01 
import Day02
import Day03
import Day04
import Day05

main :: IO ()
main = run "5b"

run :: Text -> IO ()
run "1a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input1.txt" >>= print . day01a
run "1b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input1.txt" >>= print . day01b
run "2a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input2.txt" >>= print . day02a
run "2b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input2.txt" >>= print . day02b
run "3a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input3.txt" >>= print . day03a
run "4a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input4.txt" >>= print . day04a
run "4b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input4.txt" >>= print . day04b
run "5a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input5.txt" >>= print . day05a
run "5b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input5.txt" >>= print . day05b

getInput :: FilePath -> IO String
getInput = liftM trim . readFile

trim :: String -> String
trim = trimRight . trimLeft

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse
