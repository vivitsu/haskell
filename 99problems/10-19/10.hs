-- Run-length encoding of a list.

-- Use the result of problem P09 to implement the so-called run-length encoding data compression
-- method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.

-- Using solution from problem 9.
pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack rest
pack [] = []                

encode :: Eq a => [a] -> [(a, Int)]
encode = map (\xs -> (head xs, length xs)) . pack
