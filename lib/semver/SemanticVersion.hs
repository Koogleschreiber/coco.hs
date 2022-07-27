module SemanticVersion(SemanticVersion(..), readV) where
import Data.List.Split
import Data.Char (digitToInt)

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
