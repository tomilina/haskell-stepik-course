module TypeParams where

data Shape = Circle Double | Rectangle Double Double deriving Show

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y) = if x==y then True else False
isSquare _ = False

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(***) f g ~(x,y) = (f x, g y)
-- (***) f g p = (f $ fst p, g $ snd p)

-- succ *** pred $ (5,5)
