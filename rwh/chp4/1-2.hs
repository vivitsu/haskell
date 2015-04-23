splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f l@(x:xs)
    | f x == False = splitWith f xs
    | otherwise = ys : splitWith f rest
                  where (ys, rest) = break f l
