-- Remove the K'th element from a list.

removeAt :: (Eq a) => Int -> [a] -> (Maybe a, [a])
removeAt k xs = case back of
  [] -> (Nothing, xs)
  (x:rest) -> (Just x, front ++ rest)
  where (front, back) = splitAt (k - 1) xs

