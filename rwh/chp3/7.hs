-- Define a function that joins a list of lists together using a separator value.

-- There should be a faster way than this, '++' is O(n)!
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = x
myIntersperse s (x:xs) = x ++ [s] ++ myIntersperse s xs
