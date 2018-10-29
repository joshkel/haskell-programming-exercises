module ProgrammersModule where

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show, Enum, Bounded)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum, Bounded)

data Programmer = Programmer
  { os :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

allItems :: (Enum a, Bounded a) => [a]
allItems = enumFromTo (minBound) (maxBound)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = allItems

allLanguages :: [ProgLang]
allLanguages = allItems

allProgrammers = [Programmer a b | a <- allOperatingSystems, b <- allLanguages]
