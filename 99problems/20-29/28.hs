-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves.
--    The objective is to sort the elements of this list according to their
--    length.

--    E.g. short lists first, longer lists later, or vice versa.
import Data.List (sortBy)
import Data.Ord (comparing)

lsort :: [[a]] -> [[a]]
lsort xs = sortBy compareLists xs
    where compareLists :: [a] -> [a] -> Ordering
          compareLists l1 l2
            | (length l1) < (length l2) = LT
            | (length l1) > (length l2) = GT
            | otherwise = EQ

-- Function used in the solution
lsort' :: [[a]] -> [[a]]
lsort' = sortBy (comparing length)
