module ListsFuncs where

import Data.Char

readDigits :: String -> (String, String)
readDigits = span (isDigit)
-- readDigits xs = (takeWhile (isDigit) xs, dropWhile (isDigit) xs)

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 xs = filter (pred p1 p2) xs
  where pred x y cur = x cur || y cur
