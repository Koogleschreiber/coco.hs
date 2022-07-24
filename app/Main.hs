module Main where

main :: IO ()
main = do
  putStrLn "coco"

-- import Options.Applicative
--
-- data Options = Options {
--    printHelp :: Bool
-- }
--
-- helpText = "This is coco.hs a tool to manage semantic versions and releases through the means of conventional commits."
--
-- options :: Parser Options
-- options = Options
--       <$> switch
--           ( long "help"
--          <> short 'h'
--          <> help "Print help text" )
--
-- main :: IO ()
-- main = execute =<< execParser opts
--   where
--     opts = info (options <**> helper)
--       ( fullDesc
--      <> progDesc "Parse conventional commit messages of a git repository"
--      <> header "coco.hs" )
--
-- execute :: Options -> IO ()
-- execute (Options True) = putStrLn helpText
-- execute _ = return ()
