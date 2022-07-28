module ConventionalCommit(ConventionalCommit(..), ChangeType(..), readChangeType, semver) where
import Data.List.Split (splitOn)

newtype ChangeType = ChangeType (Int, String)

semver :: [ChangeType]
semver = [
  ChangeType(0,"BreakingChange"),
  ChangeType(1,"Feature"),
  ChangeType(2,"Fix"),
  ChangeType(-1,"Refactoring")
  ]

instance Show ChangeType where
  show (ChangeType (i, n)) = show i ++ "," ++ n

instance Eq ChangeType where
  ChangeType (i, n) == ChangeType (i', n') = n == n'

instance Ord ChangeType where
  ChangeType (i, n) <= ChangeType (i', n') = i <= i'

newtype ConventionalCommit = ConventionalCommit (ChangeType, String) deriving (Eq, Show)

readChangeType :: String -> ChangeType
readChangeType s = ChangeType (i, n)
  where
  i = read(head(splitOn "," s))
  n = tail . concatMap ("," ++) . tail . splitOn  "," $ s
