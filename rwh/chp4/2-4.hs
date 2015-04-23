myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
    | f x == False = []
    | otherwise = x : myTakeWhile f xs
