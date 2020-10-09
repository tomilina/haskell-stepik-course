
--newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Read, Show, Bounded)

--instance Num a => Monoid (Sum a) where
  --mempty = Sum 0
  --Sum x `mappend` Sum y = Sum (x + y)


  newtype Xor = Xor { getXor :: Bool }
      deriving (Eq,Show)

  instance Monoid Xor where
      mempty = Xor False
      mappend (Xor x) (Xor y) = Xor (x /= y)
