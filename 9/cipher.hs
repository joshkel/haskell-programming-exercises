module Cipher where

import Data.Char

rotateChar :: String -> Int -> Char -> Char
rotateChar xs n c
    | elem c xs = chr $ ((ord c - start + n) `mod` (length xs) + start)
    | otherwise = c
    where start = ord $ head xs

rotateUpper = rotateChar ['A'..'Z']
rotateLower = rotateChar ['a'..'z']

caesar :: Int -> String -> String
caesar n xs = map rotate xs
    where rotate = (rotateUpper n) . (rotateLower n)

unCaesar :: Int -> String -> String
unCaesar n = caesar (-n)
