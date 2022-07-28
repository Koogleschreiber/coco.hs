module ConventionalCommit(ConventionalCommit(..), ChangeType(..), readChangeType) where
import Data.List.Split (splitOn)

newtype ChangeType = ChangeType (Int, String)

instance Show ChangeType where
  show (ChangeType (i, n)) = show i ++ "," ++ n

instance Eq ChangeType where
  ChangeType (i, n) == ChangeType (i', n') = n == n'

newtype ConventionalCommit = ConventionalCommit (ChangeType, String) deriving (Eq, Show)

readChangeType :: String -> ChangeType
readChangeType s = ChangeType (i, n)
  where
  i = read(head(splitOn "," s))
  n = tail . concatMap ("," ++) . tail . splitOn  "," $ s
