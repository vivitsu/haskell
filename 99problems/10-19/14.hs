-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli xs = foldr helper [] xs
    where helper x acc = (replicate 2 x) ++ acc
