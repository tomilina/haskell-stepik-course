module Distance where

data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance a@(Coord x1 y1) b@(Coord x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance a@(Coord x1 y1) b@(Coord x2 y2) = abs(x1 - x2) + abs(y1 - y2)
