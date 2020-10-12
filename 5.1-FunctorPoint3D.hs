data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf a) = Leaf (fmap f a)
  fmap f (Branch a b c) = Branch (fmap f a) (fmap f b) (fmap f c)


data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1,k2) (f v)

instance Functor (Map k1 k2) where
    fmap f (Map []) = Map []
    fmap f (Map (x:xs)) = (\x (Map xs) -> Map (x:xs)) (fmap f x) (fmap f (Map xs))
