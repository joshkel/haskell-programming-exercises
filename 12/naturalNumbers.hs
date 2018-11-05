module NaturalNumbers where

-- As natural as any
-- competitive bodybuilder
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
natIntegerToNat :: Integer -> Nat
natIntegerToNat n
    | n == 0 = Zero
    | n > 0 = Succ (natIntegerToNat (n - 1))

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n >= 0 = Just $ natIntegerToNat n
    | otherwise = Nothing
