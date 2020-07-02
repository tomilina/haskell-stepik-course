module Integral where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0 ((min a b)+h) 1
  where
    steps = 1000
    h = (max a b - min a b)/ steps
    sign = if a > b then (-1) else 1
    helper sum cur step | step < steps  = helper (sum + f cur) (cur + h) (step + 1)
                        | otherwise = ((f a + f b)/2 + sum) * h * sign
