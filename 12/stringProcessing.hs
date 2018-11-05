module StringProcessing where

import Data.Char
import Data.List
import Data.Maybe

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe s = intercalate " " $ go $ words s
  where
    go :: [String] -> [String]
    go [] = []
    go (x:xs) = (fromMaybe "a" $ notThe x) : go xs

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
isTheBeforeVowel :: String -> String -> Bool
isTheBeforeVowel a b
  | a == "the" && (isVowel $ head b) = True
  | otherwise = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go $ words s
  where
    go :: [String] -> Integer
    go [] = 0
    go (x1:(x2:xs)) =
      (toInteger $ fromEnum $ isTheBeforeVowel x1 x2) + go (x2 : xs)
    go (x1:xs) = 0

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem (toLower c) vowels

countVowels :: String -> Integer
countVowels = toInteger . length . (filter isVowel)

-- Validate Words
newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s
  | vowelCount <= consonantCount = Just $ Word' s
  | otherwise = Nothing
  where
    vowelCount = countVowels s
    consonantCount = (toInteger $ length s) - vowelCount
