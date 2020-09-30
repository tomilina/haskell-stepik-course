module Person where

data Person' = Person' String String Int

age' :: Person' -> Int
age' (Person' _ _ z) = z

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show, Eq)
--let john = Person "John" "Smith" 23
-- age john

(&) :: a -> (a -> b) -> b
x & f = f x

--john & age

updateAge :: Int -> Person -> Person
updateAge newAge person = person {age = newAge}

--matching - position and amount of arguments is important
name :: Person -> String
name (Person fn ln _) = fn ++ " " ++ ln

--position is not important, use only parameters you need
name' :: Person -> String
name' (Person {lastName = ln, firstName=fn}) = fn ++ " " ++ ln

abbrFirstName :: Person -> Person
--abbrFirstName p@Person{firstName = fn@fn1:fns} = if (length fn) <= 2 then p else p{firstName=[fn1]}
--abbrFirstName person@Person{firstName = fn1:fn2:fns} = person{firstName=[fn1]}
--abbrFirstName person = person
--abbrFirstName person = if (length $ firstName person) <= 2 then person else person{firstName=[(firstName person)!!0]}

abbrFirstName p@Person{firstName = fn} = case fn of
  (x:y:xs) -> p{firstName = [x]}
  _ -> p
