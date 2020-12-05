module Day04 where

import Data.List
import Data.List.Split
import Data.Lists
import Data.Char

day04a :: String -> Int
day04a str = countValidPassport (passportBlocks str)

-- Returns list of passport
-- Eg: 
-- [
--   ["eyr:2029 pid:157374862","byr:1991 ecl:amb hcl:#a97842 hgt:178cm"],
--   ["byr:1962 pid:547578491 eyr:2028 ecl:hzl hgt:65in iyr:2013 hcl:#623a2f"]
-- ]
passportBlocks :: String -> [[String]]
passportBlocks str = map lines (splitOn "\n\n" str)

-- Parses each passports into tuples
-- The above is converted to
-- Eg: [("hgt","71in"),("eyr","2037"),("ecl","#8e276e"),("hcl","z"),("iyr","2019"),("byr","2022"),("pid","157cm")]
parsePassport :: [String] -> [(String,String)]
parsePassport pElems = getTuples $ getElemList psswd
  where getTuples    = map (\xs -> (xs!!0, xs!!1)) 
        getElemList  = map (\x -> splitOn ":" x ) 
        psswd        = unionOf $ map (splitOn " ") pElems

isElemPresent ::String -> [(String,String)] -> Bool
isElemPresent pelem xs = pelem `elem` map fst xs 

isPassportValid :: [String] -> Bool
isPassportValid pBlock = 
  if (length passport == 8 || onlyCIDabsent)
    then True
    else False
  where passport = parsePassport pBlock
        onlyCIDabsent = (length passport == 7) && (not $ isElemPresent "cid" passport)

countValidPassport :: [[String]] -> Int
countValidPassport passports = length $ filter (==True) $ map isPassportValid passports

-----------------------------------------
isValidBYR :: String -> Bool
isValidBYR byr = if byr' >= 1920 && byr' <=2002 then True else False 
  where byr' = read byr :: Int

isValidIYR :: String -> Bool
isValidIYR iyr = if iyr' >= 2010 && iyr' <=2020 then True else False 
  where iyr' = read iyr :: Int

isValidEYR :: String -> Bool
isValidEYR eyr = if eyr' >= 2020 && eyr' <=2030 then True else False 
  where eyr' = read eyr :: Int

isValidHGT :: String -> Bool
isValidHGT hgt = if isInch then isValidInch else isValidCm
  where isInch = isInfixOf "in" hgt
        inch   = (read ( splitOn "in" hgt !! 0) :: Int)
        cm     = (read ( splitOn "cm" hgt !! 0) :: Int)
        isValidInch = if inch >= 59 && inch <= 76 then True else False
        isValidCm = if cm >= 150 && cm <= 193 then True else False

isValidHCL :: String -> Bool
isValidHCL (x:xs) = x == '#' && length xs == 6 && all isHexDigit xs

isValidECL :: String -> Bool
isValidECL ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPID :: String -> Bool
isValidPID pid = length pid == 9 && all isDigit pid 

getElemValue :: [(String,String)] -> String -> String
getElemValue tuples elemName = head [ value | (name,value) <- tuples, name == elemName] 

isPassportValid2 :: [String] -> Bool
isPassportValid2 pBlock = 
  if (length passport == 8 || onlyCIDabsent) 
    then (isValidBYR byr && isValidIYR iyr && isValidEYR eyr && isValidHGT hgt && isValidHCL hcl && isValidECL ecl && isValidPID pid)
    else False
  where passport = parsePassport pBlock
        onlyCIDabsent = (length passport == 7) && (not $ isElemPresent "cid" passport) 
        byr = getElemValue passport "byr"
        iyr = getElemValue passport "iyr"
        eyr = getElemValue passport "eyr"
        hgt = getElemValue passport "hgt"
        hcl = getElemValue passport "hcl"
        ecl = getElemValue passport "ecl"
        pid = getElemValue passport "pid"

day04b :: String -> Int
day04b str = countValidPassport2 (passportBlocks str)

countValidPassport2 :: [[String]] -> Int
countValidPassport2 passports = length $ filter (==True) $ map isPassportValid2 passports
