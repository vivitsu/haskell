-- Write a function that will determine the height of the tree.
-- The height is the largest number of hops from the root to an Empty.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node a left right) = 1 + (max (height left) (height right))
