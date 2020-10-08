data Tree a = Leaf a | Node (Tree a) (Tree a)

count :: Tree Int -> Int
count (Leaf a) = 1
count (Node l r) = 1 + count l + count r

sum1 :: Tree Int -> Int
sum1 (Leaf a) = a
sum1 (Node l r) = sum1 l + sum1 r

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go tree = (count tree, sum1 tree)
