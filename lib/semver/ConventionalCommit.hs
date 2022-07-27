module ConventionalCommit(ConventionalCommit(..), ChangeType(..)) where

newtype ChangeType = ChangeType (Int, String) deriving (Eq, Show)
newtype ConventionalCommit = ConventionalCommit (ChangeType, String) deriving (Eq, Show)


