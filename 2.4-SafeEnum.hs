module SafeEnum where

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a = if (a == maxBound) then minBound else succ a

  spred :: a -> a
  spred a = if (a == minBound) then maxBound else pred a

data Odd = Odd Integer deriving (Eq,Show)
instance Enum Odd where
    toEnum :: Int -> Odd
    toEnum m | m `mod` 2 == 0 = Odd (toInteger(m))
             | otherwise = error "toEnum: parameter cannot be odd"

    fromEnum :: Odd -> Int
    fromEnum x = fromIntegral(x)
