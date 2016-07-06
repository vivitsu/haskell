flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
    where f (Just x) (Just xs) = Just (x:xs)
          f _ _ = Nothing          
