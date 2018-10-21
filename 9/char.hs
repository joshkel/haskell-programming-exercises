module CharModule where

import Data.Char

onlyUppers :: String -> String
onlyUppers xs = filter isUpper xs


capFirst :: String -> String
capFirst [] = []
capFirst (x : xs) = (toUpper x) : xs


capAll :: String -> String
capAll [] = []
capAll (x : xs) = (toUpper x) : capAll xs


headCap :: String -> Char
headCap xs = toUpper $ head xs
headCap2 xs = (toUpper . head) xs
headCap3 = toUpper . head