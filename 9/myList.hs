module EnumModule where

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
