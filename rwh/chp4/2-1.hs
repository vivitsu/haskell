import Data.Char as Char

asInt_fold :: String -> Int
asInt_fold ('-':str) = (-1) * asInt_fold str
asInt_fold str = foldl (\acc a -> acc * 10 + Char.digitToInt a) 0 str
