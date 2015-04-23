concat :: [[a]] -> [a]
concat xss = foldr (\xs acc -> xs ++ acc) [] xss 
