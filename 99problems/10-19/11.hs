-- Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

-- Using solution from problem 10.

-- Adopted from haskell.org:

-- We need a utility type because lists in haskell are homogeneous.
-- Afterwards we use the encode function from problem 10 and map single
-- instances of a list item to Single and multiple ones to Multiple.

data ListItem a = Single a | Multiple Int a
    deriving (Show)

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack rest
pack [] = []                

encode :: Eq a => [a] -> [(a, Int)]
encode = map (\xs -> (head xs, length xs)) . pack

encodeModified :: (Eq a) => [a] ->[ListItem a]
encodeModified = map helper . encode
    where
      helper (x,1) = Single x
      helper (x,n) = Multiple n x
