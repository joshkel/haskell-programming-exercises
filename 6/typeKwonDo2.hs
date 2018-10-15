module TypeKwonDo2 where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b
    => (a -> b)
    -> Integer
    -> a
    -> b
arith f n a = (f a) + fromInteger n
