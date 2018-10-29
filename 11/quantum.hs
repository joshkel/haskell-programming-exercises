module QuantumModule where

data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

convert :: Quantum -> Bool
c1 Yes = True
c1 No = True
c1 Both = True

c2 Yes = False
c2 No = False
c2 Both = False
