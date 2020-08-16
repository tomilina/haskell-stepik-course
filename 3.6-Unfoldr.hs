module Unfoldr where

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
  helper (Just (x, ini')) = x : unfoldr f ini'
  helper Nothing          = []


revRange :: (Char,Char) -> [Char]
revRange (s, f) = unfoldr g (s, f)
  where
    g (s, f) | s > f = Nothing
             | s == f = [s]
             | otherwise = s: g (succ s, f)
