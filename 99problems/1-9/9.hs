-- Pack consecutive duplicates of list elements into sublists.

-- If a list contains repeated elements they should be placed in
-- separate sublists.

pack :: Eq a => [a] -> [[a]]
pack (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack rest
pack [] = []                
