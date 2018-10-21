module DatabaseModule where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 1
  , DbString "Hello, world!"
  , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr go []
    where go (DbDate t) xs = t : xs
          go _ xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr go []
    where go (DbNumber n) xs = n : xs
          go _ xs = xs

minDate = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr go minDate
    where go (DbDate t1) t2 = max t1 t2
          go _ t2 = t2

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr go 0
    where go (DbNumber n1) n2 = n1 + n2
          go _ n2 = n2

avgDb :: [DatabaseItem] -> Double
avgDb xs = sum / count
    where (sum, count) = foldr go (0, 0) xs
          go (DbNumber n) (sum, count) = (sum + fromIntegral n, count + 1)
          go _ (sum, count) = (sum, count)

