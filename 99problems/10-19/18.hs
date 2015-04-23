-- Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.

-- TODO: Make a version without inbuilt functions?

slice :: [a] -> Int -> Int -> [a]
slice xs i j | i > 0 = take (j - i + 1) . drop (i - 1) $ xs
