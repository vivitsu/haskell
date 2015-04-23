-- Create a function that sorts a list of lists based on the length of each sublist.
sortSubList :: (Eq a, Ord a)=> [[a]] -> [[a]]
sortSubList [] = []
sortSubList (x:xs) = let first = filter (\l -> length l <= length x) xs
                         last = filter (\l -> length l > length x) xs
                     in first ++ [x] ++ last
