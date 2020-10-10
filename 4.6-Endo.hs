module Demo where

newtype Endo a = Endo { appEndo :: a -> a}

instance Monoid (Endo a) where
  mempty = Endo id
  Endo f `mappend` Endo g = Endo (f . g)
