import Test.QuickCheck
import Test.Hspec
import Data.List
import Semver (SemanticVersion(..), readV)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

instance Arbitrary SemanticVersion where
  arbitrary = SemanticVersion <$> arbitrary

prop_read_inverse :: SemanticVersion -> Bool
prop_read_inverse s = (readV . show) s == s

prop_eq_reflex :: SemanticVersion -> Bool
prop_eq_reflex = reflexivity (==)

prop_eq_trailing_zeroes :: SemanticVersion -> Positive Int -> Bool
prop_eq_trailing_zeroes (SemanticVersion a) (Positive n) = SemanticVersion a == SemanticVersion (a ++ replicate n 0)

prop_eq_trans :: SemanticVersion -> SemanticVersion -> SemanticVersion -> Bool
prop_eq_trans a b c =
  let x = sort [a,b,c] in transitivity (==) (head x) (x!!1) (x!!2)
prop_ord_reflex :: SemanticVersion -> Bool
prop_ord_reflex = reflexivity (<=)

prop_ord_trans :: SemanticVersion -> SemanticVersion -> SemanticVersion -> Bool
prop_ord_trans a b c =
  let x = sort [a,b,c]
  in transitivity (<=) (head x) (x!!1) (x!!2)

prop_ord_antisym :: SemanticVersion -> SemanticVersion -> Bool
prop_ord_antisym a b = (a <= b && a >= b) `implies` (a == b)

implies :: Bool -> Bool -> Bool
implies a b = not a || b

reflexivity :: (a -> a -> Bool) -> a -> Bool
reflexivity f a = f a a

transitivity :: (a -> a -> Bool) -> a -> a -> a -> Bool
transitivity f a b c = (f a b && f b c) `implies` f a c

main :: IO ()
main = hspec $ do
  describe "Eq" $
    do modifyMaxSuccess (const 1000) $ it "prop_eq_reflex" $ property prop_eq_reflex
       modifyMaxSuccess (const 1000) $ it "prop_eq_trans" $  property prop_eq_trans
       modifyMaxSuccess (const 1000) $ it "prop_eq_trailing_zeroes" $ property prop_eq_trans
  describe "Ord" $
    do modifyMaxSuccess (const 1000) $ it "prop_ord_reflex" $ property prop_ord_reflex
       modifyMaxSuccess (const 1000) $ it "prop_ord_trans" $ property prop_ord_trans
       modifyMaxSuccess (const 1000) $ it "prop_ord_antisym" $ property prop_ord_antisym
  describe "Show" $
    do modifyMaxSuccess (const 1000) $ it "prop_read_inverse" $ property prop_read_inverse
