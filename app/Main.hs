module Main where
import System.Process
import Data.List.Split

main :: IO ()
main = putStrLn "Hello, Haskell!"

gitLog :: String -> IO [String]
gitLog path = splitLines <$> gitLogIO path

gitLogIO :: String -> IO String
gitLogIO path = readProcess "git" ["-C", path, "log", "--pretty=format:'%s'"] []

splitLines :: String -> [String]
splitLines = splitOn "\n"

