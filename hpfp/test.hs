sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

multiplyByPi :: Double -> Double
multiplyByPi x = 3.14 * x

area :: Double -> Double
area d = pi * (r * r)
    where r = d / 2

rvrs :: String
rvrs = drop 9 str ++ " " ++ drop 6 (take 8 str) ++ " " ++ take 5 str
    where str = "Curry is awesome"
