{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where

-- simple example
example = 1

one_a = (* 9) 6
one_b = head [(0,"doge"),(1,"kitteh")]
one_c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
one_d = if False then True else False
one_e = length [1, 2, 3, 4, 5]
one_f = (length [1, 2, 3, 4]) > (length "TACOCAT")
