exclaim x = x ++ "!"

getY x = "y"

lastWord x = last $ words x


thirdLetter :: String -> Char
thirdLetter x = x !! 3

letterIndex :: Int -> Char
letterIndex  x = text !! x
    where text = "Curry is awesome!"
