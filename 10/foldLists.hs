module MyFoldLists where

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> (f a) || b) False

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\x y -> (x == a) || y) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a = any (==a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldl (\a b -> if (f a b) == GT then a else b) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\a b -> if (f a b) == LT then a else b) (head xs) xs

