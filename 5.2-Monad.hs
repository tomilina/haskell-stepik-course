data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log ( (getMsg (f x)) ++ (getMsg (g (getValue (f x)))) ) (getValue (g (getValue (f x))))
getValue :: Log b -> b
getValue (Log _ a) = a
getMsg :: Log b -> [String]
getMsg (Log msg _) = msg
add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"
execLoggersList x (f:[]) = f x
execLoggersList x (f:fs) = (\(Log (msg:_) _) (Log ms a) -> Log (msg:ms) a) (f x) (execLoggersList (getValue (f x)) fs)

add1Log = toLogger (+1) "added one"
add1Log 3
-- Log ["added one"] 4

mult2Log = toLogger (* 2) "multiplied by 2"
mult2Log 3
-- Log ["multiplied by 2"] 6

execLoggers 3 add1Log mult2Log
--Log ["added one","multiplied by 2"] 8
