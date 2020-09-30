module Coord where

data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter w a@(Coord x y) = Coord (x*w + (w/2) (y*w + w/2) :: Coord Double

--getCell :: Double -> Coord Double -> Coord Int
--getCell w (Coord x y) = Coord ((mod x w)::Int) ((mod y w)::Int)
