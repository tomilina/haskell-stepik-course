module EnumTypes

data B = T | F deriving Show

not' :: B -> B
not' T = F
not' F = T
