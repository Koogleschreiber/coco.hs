module SemanticVersion(SemanticVersion(..), readV, apply, toList) where
import Data.List.Split
import Data.Char (digitToInt)
import ConventionalCommit (ChangeType (ChangeType))

newtype SemanticVersion = SemanticVersion [Int]

readV :: String -> SemanticVersion
readV a = SemanticVersion(map (\s -> read s :: Int) (splitOn "." a))

instance Eq SemanticVersion where
 SemanticVersion [] == SemanticVersion [] = True
 SemanticVersion [] == SemanticVersion (x:xs)
  | x == 0 = SemanticVersion [] == SemanticVersion xs
  | otherwise = False
 SemanticVersion x == SemanticVersion [] = SemanticVersion [] == SemanticVersion x
 SemanticVersion (a:as) == SemanticVersion (b:bs)
  | a == b = SemanticVersion as == SemanticVersion bs
  | otherwise = a == b

instance Show SemanticVersion where
  show (SemanticVersion []) = "0"
  show (SemanticVersion a) = tail(concatMap ((++) "." . show) a)

instance Ord SemanticVersion where
 (SemanticVersion a) <= (SemanticVersion b)
  | SemanticVersion a == SemanticVersion b = True
  | null a = SemanticVersion [0] <= SemanticVersion b
  | null b = SemanticVersion a <= SemanticVersion [0]
  | head a == head b = SemanticVersion (tail a) <= SemanticVersion (tail b)
  | otherwise = head a <= head b

toList :: SemanticVersion -> [Int]
toList (SemanticVersion a) = a

apply :: SemanticVersion -> ChangeType -> SemanticVersion
apply (SemanticVersion a) (ChangeType (number, _)) = SemanticVersion (applyForList a number)

applyForList :: [Int] -> Int -> [Int]
applyForList (x:xs) 0 = (x + 1) : map (*0) xs
applyForList (x:xs) n = x : applyForList xs (n-1)
applyForList xs _ = xs
