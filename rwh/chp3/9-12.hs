data Point = Point Double Double deriving (Show)
data Vector = Vector Point Point
data Direction = LeftTurn | RightTurn | Straight deriving (Show)

-- Returns `Direction` for any given 3 points
direction :: Point -> Point -> Point -> Direction
direction (Point x1 y1) (Point x2 y2) (Point x3 y3)
    | theta > 0 = LeftTurn
    | theta < 0 = RightTurn
    | otherwise = Straight
    where theta = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 - x1))

directionFromList :: [Point] -> [Direction]
directionFromList (a:b:[]) = []
directionFromList (a:b:c:xs) = direction a b c : directionFromList (b:c:xs)
