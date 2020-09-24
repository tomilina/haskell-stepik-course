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




import Data.Time.Clock
import Data.Time.Format
--import System.Locale

timeToString :: UTCTime -> String
timeToString = undefined --formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString l = "%l"

logEntryToString :: LogEntry -> String
logEntryToString l = "%timeToString(timestamp l): logLevelToString(logLevel l): message l"
