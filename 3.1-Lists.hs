module Lists where

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y lst = x : y : lst

second :: [a] -> a
second = head . tail

second' :: [a] -> a
second' (_: xs) = head xs

second'' :: [a] -> a
second'' (_: x: _) = x

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) = if odd x then x: oddsOnly xs else oddsOnly xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome  (x:xs) | last xs == x = True && (isPalindrome $ init xs)
                     | otherwise = False


--unzip :: [(a,b)] -> ([a], [b])
--unzip [] = ([], [])
--unzip ((x, y):xys) =
  --let (xs, ys) = unzip xys
  --in (x: xs, y: ys)


sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] ys = ys
sum2 xs [] = xs
sum2 (x:xs) (y:ys) = (x+y) : sum2 xs ys


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 x y z = sum2 x (sum2 y z)


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) = group [] [x] xs
 where group :: Eq a => [[a]] -> [a] ->[a] -> [[a]]
       group res cur [] = res ++ [cur]
       group res cur (t:ts) = if ((head cur) == t) then (group res (t:cur) ts) else (group (res ++ [cur]) [t] ts)
