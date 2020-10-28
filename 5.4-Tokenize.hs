-- data Token = Number Int | Plus | Minus | LeftBrace | RightBrace    
--     deriving (Eq, Show)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
    deriving (Eq, Show)

import Data.Char

asToken :: String -> Maybe Token
asToken s = case x of all isDigit x -> Just (Number (read x :: Int))
                           x == "+" -> Just Plus
                           x == "-" -> Just Minus
                           x == "(" -> Just LeftBrace
                           x == ")" -> Just RightBrace
                           otherwise -> Nothing

tokenize :: String -> Maybe [Token]
tokenize input = undefined
