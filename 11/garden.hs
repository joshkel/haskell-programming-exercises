module GardenModule where

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden =
  Garden Gardener
         FlowerType
  deriving (Show)

data GardenAsSumOfProducts
  = GardeniaASOP Gardener
  | DaisyASOP Gardener
  | RoseASOP Gardener
  | LilacASOP Gardener
