module FindDigit where

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit [x] = if isDigit x then Just x else Nothing
findDigit (x: xs) = if isDigit x then Just x else findDigit xs
