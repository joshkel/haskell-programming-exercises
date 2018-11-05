module MyMarvelousMaybeModule where

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (: [])

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> let xs = take 3 $ repeat Nothing
-- >>> catMaybes xs
-- []
catMaybes :: [Maybe a] -> [a]
-- Option 1: From scratch, recursing ourselves
{-
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : (catMaybes xs)
-}
-- Option 2: foldr
catMaybes = foldr go []
  where
    go Nothing xs = xs
    go (Just a) xs = a : xs

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
-- (later called `sequence`)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where
    go Nothing _ = Nothing
    go _ Nothing = Nothing
    go (Just a) (Just xs) = Just (a : xs)

-- Either!
lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where
    go (Left a) xs = a : xs
    go _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where
    go (Right b) xs = b : xs
    go _ xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where
    go (Left a) (as, bs) = (a : as, bs)
    go (Right b) (as, bs) = (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa fb (Left a) = fa a
either' fa fb (Right b) = fb b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\a -> Nothing) (\b -> Just (f b))
