myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy eq xs = foldl eqHelper [] xs
    where eqHelper [] x  = [[x]]
          eqHelper acc x | eq (head (last acc)) x = (init acc) ++ [((last acc) ++ [x])]
                         | otherwise = acc ++ [[x]]
