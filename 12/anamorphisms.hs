module Anamorphisms where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f (f a))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  let result = f b
   in case result of
        Nothing -> []
        Just (a, b') -> a : (myUnfoldr f b')

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr go a
  where
    go b = Just (b, b')
      where
        b' = f b

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a =
  let result = f a
   in case result of
        Nothing -> Leaf
        Just (a1, b', a2) -> Node (unfold f a1) b' (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
  unfold
    (\a ->
       if a < n
         then Just (a + 1, a, a + 1)
         else Nothing)
    0
