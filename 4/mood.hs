module MoodModule where

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood    _ = Blah

main = do
    print $ changeMood Woot
    print $ changeMood Blah
