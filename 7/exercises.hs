module ExercisesModule where


tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

nthDigit :: Integral a => a -> a -> a
nthDigit n x = xLast `mod` n
    where xLast = x `div` n

hundredsDigit = nthDigit 100


foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

foldBool3' x y b =
    case b of
        True -> y
        False -> x

foldBool3'' x y b
    | b         = y
    | otherwise = x


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f(a), c)


roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show

main = do
    print (roundTrip 4)
    print (roundTrip' 4)
    print ((roundTrip3 4) :: Integer)
    print (id 4)
