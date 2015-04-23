lastButOne :: [a] -> a
lastButOne [] = error "Empty list"
lastButOne [x] = error "Singleton list"
lastButOne [x,_] = x
lastButOne (_:xs) = lastButOne xs

