module Roots where

roots :: Double -> Double -> Double -> Either [Char] (Double, Double)
roots a b c
  | discr >= 0 Right (x1, x2)
  | otherwise = Left "Negative discriminant"
  where
    x1 = helper (-d)
    x2 = helper d
    helper x = (-b + x) / (2*a)
    d = sqrt discr
