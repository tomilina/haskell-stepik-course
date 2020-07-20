module ListsFuncs where

import Data.Char

readDigits :: String -> (String, String)
readDigits = span (isDigit)
-- readDigits xs = (takeWhile (isDigit) xs, dropWhile (isDigit) xs)

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 xs = filter (pred p1 p2) xs
  where pred x y cur = x cur || y cur

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort xs = qsort(filter (< head xs) xs) ++ filter (== head xs) xs ++ qsort(filter (> head xs) xs)

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms xs = concatMap (\x -> map (x:) (perms (filter (/=x) xs))) xs

import Data.Char
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words
