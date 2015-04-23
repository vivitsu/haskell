-- Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

data ListItem a = Single a | Multiple Int a
    deriving (Show)

modifiedPack :: Eq a => [a] -> [(a, Int)]
modifiedPack (x:xs) = let (first, rest) = span (==x) xs
                      in (x, (length first) + 1) : modifiedPack rest
modifiedPack [] = []

-- encode :: Eq a => [a] -> [(a, Int)]
-- encode = map (\xs -> (head xs, length xs)) . pack

encodeDirect :: (Eq a) => [a] ->[ListItem a]
encodeDirect = map helper . modifiedPack
    where
      helper (x,1) = Single x
      helper (x,n) = Multiple n x
