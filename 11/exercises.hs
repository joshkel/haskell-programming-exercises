module ChapterExercises where

import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] ys = True
isSubseqOf xs [] = False
isSubseqOf xs@(x:xs2) ys@(y:ys2) =
  case x == y of
    True -> isSubseqOf xs2 ys2
    False -> isSubseqOf xs ys2

testIsSubseqOf :: IO ()
testIsSubseqOf = do
  check "blah" "blahwoot"
  check "blah" "wootblah"
  check "blah" "wboloath"
  check "blah" "wootbla"
  check "blah" "halbwoot"
  check "blah" "blawhoot"
  where
    check x y =
      putStrLn
        ("isSubseqOf \"" ++
         x ++ "\" \"" ++ y ++ "\" = " ++ (show $ isSubseqOf x y))

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x) : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map go $ words xs
  where
    go cs@(c:s) = (c : s, toUpper c : s)

-- Source: https://stackoverflow.com/a/4981265/25507
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

capitalizeParagraph :: String -> String
capitalizeParagraph xs = snd $ foldl go (True, "") xs
  where
    go :: (Bool, String) -> Char -> (Bool, String)
    go (isStart, s) x
      | x == '.' = (True, s ++ [x])
      | isStart && (isLetter x) = (False, s ++ [toUpper x])
      | otherwise = (isStart, s ++ [x])
