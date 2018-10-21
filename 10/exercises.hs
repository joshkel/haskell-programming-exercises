module Exercises where

stops  = "pbtdkg"
vowels = "aeiou"

quasiWords = [[x, y, z] | x <- stops, y <- vowels, z <- stops]
quasiPWords = [['p', y, z] | y <- vowels, z <- stops]
quasiPWords2 = filter (\x -> head x == 'p') quasiPWords

seekritFunc x = div (sum (map length (words x))) (length (words x))
seekritFunc2 x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

