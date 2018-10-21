{-# OPTIONS_GHC -Wincomplete-patterns #-}
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Not a digit"

digits :: Int -> [Int]
digits n
    | n == 0    = []
    | n < 0     = digits (abs n)
    | otherwise = digits d ++ [m]
        where (d, m) = n `divMod` 10

hyphenJoin = concat . (intersperse "-")

wordNumber :: Int -> String
wordNumber n = hyphenJoin (map digitToWord (digits n))
