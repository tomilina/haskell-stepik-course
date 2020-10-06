data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n-1))

add :: Nat -> Nat -> Nat
add Zero b = b
add a Zero = a
add a b = toNat (fromNat a + fromNat b)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul a b = toNat (fromNat a * fromNat b)

fac :: Nat -> Nat
fac Zero = Suc Zero
fac (Suc Zero) = Suc Zero
fac (Suc n) = toNat (fromNat(fac n) * fromNat (Suc n))

-----

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc n) = mul n1 (fac n)
