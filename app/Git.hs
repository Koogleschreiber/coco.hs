module Git where
import System.Process
import Data.List.Split

type CommitMessage = String

gitLog :: String -> IO [CommitMessage]
gitLog path = splitLines <$> gitLogIO path

gitLogIO :: String -> IO String
gitLogIO path = readProcess "git" ["-C", path, "log", "--pretty=format:'%s'"] []

splitLines :: String -> [CommitMessage]
splitLines = splitOn "\n"

