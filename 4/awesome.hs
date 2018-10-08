module Awesome where

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: (Num a, Ord a) => a -> a
myAbs x = if x < 0 then -x else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f (a, b) (c, d) = ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
