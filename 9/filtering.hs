module FilteringModule where

import Data.Bool

maybeMinus x = bool x (-x) (x == 3)

multiplesOfThree = filter (\x -> rem x 3 == 0) [1..30]

isArticle "a" = True
isArticle "an" = True
isArticle "the" = True
isArticle _ = False

noArticles xs = filter (not . isArticle) $ words xs
