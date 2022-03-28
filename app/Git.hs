module Git where
import System.Process
import Data.List.Split

type CommitMessage = String

gitLog :: String -> IO [CommitMessage]
gitLog path = splitLines <$> gitLogText path

gitLogText :: String -> IO String
gitLogText path = readProcess "git" ["-C", path, "log", "--pretty=format:'%s'"] []

splitLines :: String -> [CommitMessage]
splitLines = splitOn "\n"
