module ScansModule where

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibsFirst20 = take 20 fibs
fibsUnder100 = takeWhile (<100) fibs

factorial = scanl (*) 1 [1..]

