import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString l = show l

logEntryToString :: LogEntry -> String
logEntryToString l = timeToString(timestamp l) ++ ": " ++ logLevelToString(logLevel l) ++ ": " ++ message l
