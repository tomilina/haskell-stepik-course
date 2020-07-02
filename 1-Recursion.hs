module Recursion where

factorial n = if n == 0 then 1 else n * factorial (n - 1)

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

factorial'' 0 = 1
factorial'' n = if n < 0 then error "arg must be >= 0" else n * factorial'' (n - 1)

-- охранное выражение
factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be >= 0"
               | n > 0 = n * factorial''' (n - 1)

factorial4 :: Integer -> Integer
factorial4 n | n ==0 = 1
             | n > 0 = n * factorial4 (n - 1)
             | otherwise = error "arg must be >= 0"

fibonacci' :: Integer -> Integer
fibonacci' n | n == 0 = 0
             | n == 1 = 1
             | n > 0 = fibonacci' (n - 1) + fibonacci' (n - 2)
             | n < 0 = fibonacci' (n + 2) - fibonacci' (n + 1)

-- tail recursion
factorial5 n | n >= 0 = helper 1 n
             | otherwise = error "arg must be >= 0"

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


fibonacci :: Integer -> Integer
fibonacci n = helper' 0 1 n

helper' acc1 acc2 n | n == 0 = acc1
                    | n == 1 = acc2
                    | n > 0 = helper' acc2 (acc1 + acc2) (n - 1)
                    | n < 0 = helper' (acc2 - acc1) acc1 (n + 1)

-- let in
seqA :: Integer -> Integer
seqA n =
        let
          helper acc1 acc2 acc3 n | n == 0 = acc1
                                  | n == 1 = acc2
                                  | n == 2 = acc3
                                  | n > 0 = helper acc2 acc3 (acc2 + acc3 - 2*acc1) (n - 1)
        in helper 1 2 3 n

-- where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
              | x > 0 = helper 0 0 x
              | x < 0 = helper 0 0 (-x)
              where
                helper :: Integer -> Integer -> Integer -> (Integer, Integer)
                helper sum q 0 = (sum, q)
                helper sum q n = helper (sum + mod n 10) (q + 1) (div n 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0 a 0
  where
    helper sum cur count = helper (sum + f cur) (cur + h) (count + 1)
    h = (a - b)/1000
