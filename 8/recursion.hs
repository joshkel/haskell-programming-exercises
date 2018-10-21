{-# OPTIONS_GHC -Wincomplete-patterns #-}
module RecursionModule where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d     = (count, n)
            | otherwise = go (n - d) d (count + 1)


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"


sumUp :: (Eq a, Num a) => a -> a
sumUp 0
    | n == 0 = 0
    | otherwise = n + sumUp(n - 1)


multiplyUp :: (Integral a) => a -> a -> a
multiplyUp a b
    | b > 0     = a + multiplyUp a (b - 1)
    | b < 0     = (-a) + multiplyUp a (b + 1)
    | otherwise = 0


data DividedResult =
     Result Integer
   | DividedByZero
   deriving (Show, Eq)

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' num 0 = DividedByZero
dividedBy' num denom = Result $ (signum num) * (signum denom) * go (abs num) (abs denom) 0
    where
        go n d count
            | n < d     = count
            | otherwise = go (n - d) d (count + 1)

dividedBy'' :: Integer -> Integer -> DividedResult
dividedBy'' num denom
    | denom == 0           = DividedByZero
    | num < 0 && denom < 0 = Result $ go (-num) (-denom) 0
    | num < 0 || denom < 0 = Result $ negate $ go (abs num) (abs denom) 0
    | otherwise            = Result $ go num denom 0
    where
        go n d count
            | n < d     = count
            | otherwise = go (n - d) d (count + 1)


mc91 :: Integral a => a -> a
mc91 x
    | x > 100   = x - 10
    | otherwise = mc91(mc91(x + 11))
