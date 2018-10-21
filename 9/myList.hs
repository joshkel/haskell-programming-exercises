module MyListModule where

-- Part 1: Enums
eftBool :: Bool -> Bool -> [Bool]
eftBool from to
    | from > to  = []
    | from == to = [to]
    | otherwise  = from : eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
    | from > to  = []
    | from == to = [to]
    | otherwise  = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
    | from > to  = []
    | from == to = [to]
    | otherwise  = from : eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to
    | from > to  = []
    | from == to = [to]
    | otherwise  = from : eftChar (succ from) to


-- Part 2: List functions
myTake :: Int -> [a] -> [a]
myTake 0 xs = []
myTake n [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

myWords :: String -> [String]
myWords "" = []
myWords (' ' : xs) = myWords xs
myWords xs = (takeWhile (/=' ') xs) : (myWords $ dropWhile (/=' ') xs)


myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


-- Part 3: Zip
myZip :: [a] -> [b] -> [(a, b)]
myZip [] bs = []
myZip as [] = []
myZip (a : as) (b : bs) = (a, b) : (myZip as bs)


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] bs = []
myZipWith f as [] = []
myZipWith f (a : as) (b : bs) = (f a b) : (myZipWith f as bs)


myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 as bs = myZipWith (\a b -> (a, b)) as bs


-- Part 4: Chapter exercises
myOr :: [Bool] -> Bool
myOr [] = True
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = (f x) || (myAny f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs) = if a == x then True else myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a xs = any (==a) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = if f x xsMax == GT then x else xsMax
    where xsMax = myMaximumBy f xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = if f x xsMin == LT then x else xsMin
    where xsMin = myMinimumBy f xs


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
