module Main where

import Git
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options {
  path :: String, quiet :: Bool
}

options :: Parser Options
options = Options
      <$> strOption
          ( long "path"
         <> short 'p'
         <> help "Path to the git repository" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )

main :: IO ()
main = execute =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Parse conventional commit messages of a git repository"
     <> header "coco.hs" )

execute :: Options -> IO ()
execute (Options path False) = gitLog path >>= mapM_ putStrLn
execute _ = return ()
