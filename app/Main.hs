{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text ( Text )
import Data.Char (isSpace)
import Control.Monad (liftM)

import Day01 
import Day02

main :: IO ()
main = run "2b"

run :: Text -> IO ()
run "1a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input1.txt" >>= print . day01a
run "1b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input1.txt" >>= print . day01b
run "2a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input2.txt" >>= print . day02a
run "2b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input2.txt" >>= print . day02b

getInput :: FilePath -> IO String
getInput = liftM trim . readFile

trim :: String -> String
trim = trimRight . trimLeft

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse
