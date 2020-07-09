module Lists where

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y lst = x : y : lst

second :: [a] -> a
second = head . tail

second' :: [a] -> a
second' (_: xs) = head xs

second'' :: [a] -> a
second'' (_: x: _) = x
