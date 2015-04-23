-- Flatten a nested list structure.

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list
-- with its elements (recursively).

-- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []
