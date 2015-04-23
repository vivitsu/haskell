-- Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

data ListItem a = Single a | Multiple Int a
    deriving (Show)

decodeModified :: [ListItem a] -> [a]
decodeModified xs = foldr decode [] xs
    where decode (Single a) acc = a:acc
          decode (Multiple n a) acc = (replicate n a) ++ acc
