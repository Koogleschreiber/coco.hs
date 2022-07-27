module ConventionalCommit(ConventionalCommit(..), ChangeType(..), readChangeType) where
import Data.List.Split (splitOn)

newtype ChangeType = ChangeType (Int, String) deriving (Eq)

instance Show ChangeType where
  show (ChangeType (i, n)) = show i ++ "," ++ n

newtype ConventionalCommit = ConventionalCommit (ChangeType, String) deriving (Eq, Show)


readChangeType :: String -> ChangeType
readChangeType s = ChangeType (i, n)
  where
  i = read(head(splitOn "," s))
  n = tail . concatMap ("," ++) . tail . splitOn  "," $ s
