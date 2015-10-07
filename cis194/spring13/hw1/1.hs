-- Validating credit card numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0 = []
              | otherwise = (x `rem` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft = zipWith ($) (cycle [id, (*2)])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldr step 0
    where step x acc
            | x >= 10 = acc + q + r
            | otherwise = acc + x
            where q = x `div` 10
                  r = x `rem` 10

validate :: Integer -> Bool
validate x = (helper `rem` 10) == 0
  where helper = sumDigits . doubleEveryOther . toDigits $ x

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
