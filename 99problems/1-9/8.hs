-- Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced
-- with a single copy of the element.

-- The order of the elements should not be changed.

compress :: Eq a => [a] -> [a]
compress x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x
