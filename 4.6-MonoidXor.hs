
--newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Read, Show, Bounded)

--instance Num a => Monoid (Sum a) where
  --mempty = Sum 0
  --Sum x `mappend` Sum y = Sum (x + y)


  newtype Xor = Xor { getXor :: Bool }
      deriving (Eq,Show)

  instance Monoid Xor where
      mempty = Xor False
      mappend (Xor x) (Xor y) = Xor (x /= y)

-- pair Monoid
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (x1, x2) `mappend` (y1, y1) = (x1 `mappend` y1, x2 `mappend` y2)

-- maybe Monoid
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
