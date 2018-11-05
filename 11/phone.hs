{-# LANGUAGE NamedFieldPuns #-}

module PhoneExercise where

import Data.Char
import Data.List
import Data.Maybe

data DaPhone =
  DaPhone [PhoneKey]

-- validButtons = "1234567890*#"
type Digit = Char

data PhoneKey = PhoneKey
  { key :: Digit
  , chars :: String
  } deriving (Eq, Show)

capitalize = '^'

phone =
  DaPhone
    [ PhoneKey '2' "ABC"
    , PhoneKey '3' "DEF"
    , PhoneKey '4' "GHI"
    , PhoneKey '5' "JKL"
    , PhoneKey '6' "MNO"
    , PhoneKey '7' "PQRS"
    , PhoneKey '8' "TUV"
    , PhoneKey '9' "WXYZ"
    , PhoneKey '*' [capitalize]
    , PhoneKey '0' "+ _"
    , PhoneKey '#' ".,"
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- Valid presses: 1 and up
type Presses = Int

findPresses :: PhoneKey -> Char -> Maybe Int
findPresses PhoneKey {key, chars} c = do
    n <- elemIndex (toUpper c) $ chars
    Just $ n + 1

findKey :: DaPhone -> Char -> Maybe PhoneKey
findKey (DaPhone phoneKeys) c =
  find (\key -> isJust (findPresses key c)) phoneKeys

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone c
  | isUpper c =
    (reverseTaps phone capitalize) ++ (reverseTaps phone $ toLower c)
  | otherwise = fromMaybe [] $ go phone c
  where
    go phone c = do
      k <- findKey phone c
      n <- findPresses k c
      Just [(key k, n)]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone s = (concat . map (reverseTaps phone)) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

countRuns :: Eq a => [a] -> [(a, Int)]
countRuns [] = []
countRuns (x:xs) = (x, 1 + length moreX) : countRuns rest
    where (moreX, rest) = span (==x) xs

mostPopular :: (Eq a, Ord a) => [a] -> a
mostPopular = fst . head . sortOn (negate . snd) . countRuns . sort

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular . (filter isLetter)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular . words . concat
