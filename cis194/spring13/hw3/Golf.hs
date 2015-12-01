skip :: Int -> [a] -> [a]
skip n xs = case drop (n-1) xs of
    (y:ys) -> y : skip n ys
    []     -> []

skips ::  [a] -> [[a]]
skips xs = [ skip n xs | n <- [1..(length xs)] ]

localMaxima :: [Integer] -> [Integer]
localMaxima l@(x:y:z:xs) = map second $ filter isMaxima (zip3 l (y:z:xs) (z:xs))
    where isMaxima (x, y, z) | y > x && y > z = True
                             | otherwise = False
          second (a, b, c) = b
