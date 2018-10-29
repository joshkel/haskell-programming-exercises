module Cipher where

import Data.Char

rotateChar :: String -> Int -> Char -> Char
rotateChar xs n c
  | elem c xs = chr $ ((ord c - start + n) `mod` (length xs) + start)
  | otherwise = c
  where
    start = ord $ head xs

rotateUpper = rotateChar ['A' .. 'Z']

rotateLower = rotateChar ['a' .. 'z']

letterOrd c
    | isUpper c = (ord c) - (ord 'A')
    | isLower c = (ord c) - (ord 'a')

rotateVigenere keyword i = (rotateUpper n) . (rotateLower n)
    where n = letterOrd (keyword !! (i `mod` length keyword))

vigenere :: String -> String -> String
vigenere keyword xs = map rotate (zip [0..] xs)
    where rotate (i, c) = rotateVigenere keyword i c
