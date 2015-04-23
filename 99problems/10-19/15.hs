-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli xs n = foldr helper [] xs
    where helper x acc = (replicate n x) ++ acc
