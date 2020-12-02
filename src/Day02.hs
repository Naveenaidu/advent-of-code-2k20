module Day02 where

import Text.ParserCombinators.Parsec

data PswdPolicy 
  = PswdPolicy
  { ppMin   :: Int
  , ppMax   :: Int
  , ppChar   :: Char
  , ppWord  :: String
  } deriving (Show, Eq)

-- day02a :: String -> [String]
-- day02a str = lines str

day02a :: String -> Int
day02a str = 
  case parse parseInput "" str of
    Left _ -> -1
    Right pswdPolicies -> calcValidPasswords pswdPolicies

-- Line Example: "4-12 r: rrrzrgkrrrrkr"
-- Parses the above line into
-- ppMin = 4, ppMax = 12, ppStr = r, ppWord = rrrzrgkrrrrkr
parsePassword :: Parser PswdPolicy
parsePassword = do
  ppmin <- many1 (noneOf "-")
  char '-'
  ppmax <- many1 (noneOf " ")
  char ' '
  ppStr <- many1 (noneOf ":")
  string ": "
  ppWord <- many1 (noneOf "\n")
  return (PswdPolicy (read ppmin :: Int) (read ppmax :: Int) (ppStr!!0) ppWord)

-- Convert the lines into PswdPolicy type
parseInput :: Parser [PswdPolicy]
parseInput = do
  pswdPolicies <- many parsePassword
  return pswdPolicies

calcValidPasswords :: [PswdPolicy] -> Int
calcValidPasswords pswdPolicies = length validPasswordList
  where validPasswordList = filter (==True) $ map isValidPassword pswdPolicies

countChar :: Char -> String -> Int
countChar char str = length $ filter (==char) str

-- Calculate the number of times the ppStr has occured in the word and then check if it lies
-- between ppmin and ppmax.
-- If valid return True else False
isValidPassword :: PswdPolicy -> Bool
isValidPassword (PswdPolicy ppmin ppmax char pswd) =
  if charCount >= ppmin && charCount <= ppmax
    then True
    else False
  where charCount = countChar char pswd

-------------------------------------------------------------

-- Note: The naming of the fields ppmin and ppmax should be different, but my brain is zoned
-- out to think of better names X X
-- Extract the characters at ppmax and ppmin position from pswd and compare it with char.
-- `XOR` the boolead values at both position, this will help us get passwords only when
-- one of then is correct.
-- XOR can be simply written as `a /= b`
-- Return Value True mean the password is valid
isValidPassword2 :: PswdPolicy -> Bool
isValidPassword2 (PswdPolicy ppmin ppmax char pswd) = 
  isCharAtppmin /=  isCharAtppmax
  where isCharAtppmin = pswd !! (ppmin-1) == char
        isCharAtppmax = pswd !! (ppmax-1) == char

calcValidPasswords2 :: [PswdPolicy] -> Int
calcValidPasswords2 pswdPolicies = length validPasswordList
  where validPasswordList = filter (==True) $ map isValidPassword2 pswdPolicies

day02b :: String -> Int
day02b str = 
  case parse parseInput "" str of
    Left _ -> -1
    Right pswdPolicies -> calcValidPasswords2 pswdPolicies
