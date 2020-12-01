{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Text ( Text )
import Data.Char (isSpace)
import Control.Monad (liftM)

import Day01 

main :: IO ()
main = run "1b"

run :: Text -> IO ()
run "1a" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input1.txt" >>= print . day01a
run "1b" = getInput "/home/theprophet/personal/advent-of-code-2k20/data/input1.txt" >>= print . day01b

getInput :: FilePath -> IO String
getInput = liftM trim . readFile

trim :: String -> String
trim = trimRight . trimLeft

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse
