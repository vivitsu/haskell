-- Write a function that computes the mean of a list, i.e.
-- the sum of all elements in the list divided by its length.

-- (You may need to use the fromIntegral function to convert the length of the
-- list from an integer into a floating point number.)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

mean :: [Int] -> Float
mean [] = error "Empty list!"
mean xs = (fromIntegral (mySum xs)) / (fromIntegral (myLength xs)) 
