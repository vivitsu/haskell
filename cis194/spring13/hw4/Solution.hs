fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/=1)
        . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node n l b r) | even n    = Node (n+1) (insert a l) b r
                        | otherwise = Node (n+1)  l b (insert a r)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr f False
    where f True = not
          f False = id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr g []
    where g a b = f a : b

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) | x `elem` xs   = rmdups xs
              | otherwise     = x : rmdups xs

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [ (x,y) | x <- xs, y <- ys ]

sieveOfSundaram :: Integer -> [Integer]
sieveOfSundaram n = rmdups . map ((\x -> 2*x + 1) . snd) . filter f $ cartProd [1..n] [1..n]
    where f (x, y) = not ((1 <= x && x <= y && 1<= y) && ((x + y + 2*x*y) <= n))
