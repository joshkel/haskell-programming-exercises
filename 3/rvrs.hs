module Reverse where

rvrs :: String -> String
rvrs x = concat [drop lastN x, take rest $ drop firstN x, take firstN x]
    where firstN = 5
          lastN = length x - 7
          rest = lastN - firstN

main :: IO ()
main = print $ rvrs "Curry is awesome"
