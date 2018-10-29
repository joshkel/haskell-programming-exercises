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

letterOrd :: Char -> Int
letterOrd c
  | isUpper c = (ord c) - (ord 'A')
  | isLower c = (ord c) - (ord 'a')

shouldRotate :: Char -> Bool
shouldRotate c = (isUpper c) || (isLower c)

rotateVigenere keyword i = (rotateUpper n) . (rotateLower n)
  where
    n = letterOrd (keyword !! (i `mod` length keyword))

vigenere :: String -> String -> String
vigenere keyword xs = snd $ foldl go (0, "") xs
  where
    go :: (Int, String) -> Char -> (Int, String)
    go (i, s) c
      | shouldRotate c = (i + 1, s ++ [rotateVigenere keyword i c])
      | otherwise = (i, s ++ [c])
