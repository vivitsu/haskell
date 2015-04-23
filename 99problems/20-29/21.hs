-- Insert an element at a given position into a list.

-- Using inbuilt functions
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = take (k-1) xs ++ [x] ++ drop (k-1) xs

-- Using the function `split` that we defined in problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split l@(x:xs) n | n > 0 = (x:ys, zs)
                 | otherwise = ([], l)
  where (ys, zs) = split xs (n - 1)

insertAt' :: a -> [a] -> Int -> [a]
insertAt' x xs n = let (ys, zs) = split xs (n-1) in ys ++ (x:zs)
