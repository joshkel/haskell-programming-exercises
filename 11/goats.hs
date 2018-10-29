{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}

module GoatsModule where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int
  deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, s) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n1, n2) = tooMany (n1 + n2)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = tooMany (n1 + n2)

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Eq, Show)

papu = Person "Papu" 5
-- age papu
-- name papu