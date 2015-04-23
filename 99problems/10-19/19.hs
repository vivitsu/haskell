-- Rotate a list N places to the left.

-- TODO: Make a version without inbuilt functions?

rotate :: [a] -> Int -> [a]
rotate xs n = (drop n xs) ++ (take n xs)
