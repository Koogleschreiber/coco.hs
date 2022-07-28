module SemanticVersionSpec where
import Test.QuickCheck
import Test.Hspec
import Data.List
import RelationLaws (implies, transitivity, reflexivity)
import SemanticVersion (SemanticVersion(..), readV, apply)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import ConventionalCommit (ChangeType(ChangeType))

instance Arbitrary SemanticVersion where
  arbitrary = SemanticVersion <$> arbitrary

prop_read_inverse :: SemanticVersion -> Bool
prop_read_inverse s = (readV . show) s == s

prop_eq_reflex :: SemanticVersion -> Bool
prop_eq_reflex = reflexivity (==)

prop_eq_trailing_zeroes :: SemanticVersion -> Positive Int -> Bool
prop_eq_trailing_zeroes (SemanticVersion a) (Positive n) = SemanticVersion a == SemanticVersion (a ++ replicate n 0)

prop_eq_trans :: SemanticVersion -> SemanticVersion -> SemanticVersion -> Bool
prop_eq_trans = transitivity (==)
prop_ord_reflex :: SemanticVersion -> Bool
prop_ord_reflex = reflexivity (<=)

prop_ord_trans :: SemanticVersion -> SemanticVersion -> SemanticVersion -> Bool
prop_ord_trans = transitivity (<=)

prop_ord_antisym :: SemanticVersion -> SemanticVersion -> Bool
prop_ord_antisym a b = (a <= b && a >= b) `implies` (a == b)

doesNotIncrement :: SemanticVersion -> NonPositive Int -> Bool
doesNotIncrement v b = v `apply` ChangeType (getNonPositive b-1, "") == v

doesIncrement :: SemanticVersion -> Positive Int -> Bool
doesIncrement v b = v `apply` ChangeType (getPositive b, "") >= v

semanticVersionSuite = do
  describe "SemanticVersion Eq" $
    do modifyMaxSuccess (const 500) $ it "prop_eq_reflex" $ property prop_eq_reflex
       modifyMaxSuccess (const 500) $ it "prop_eq_trans" $  property prop_eq_trans
       modifyMaxSuccess (const 500) $ it "prop_eq_trailing_zeroes" $ property prop_eq_trailing_zeroes
  describe "SemanticVersion Ord" $
    do modifyMaxSuccess (const 500) $ it "prop_ord_reflex" $ property prop_ord_reflex
       modifyMaxSuccess (const 500) $ it "prop_ord_trans" $ property prop_ord_trans
       modifyMaxSuccess (const 500) $ it "prop_ord_antisym" $ property prop_ord_antisym
  describe "SemanticVersion Show" $
    do modifyMaxSuccess (const 500) $ it "prop_read_inverse" $ property prop_read_inverse
  describe "SemanticVersion apply" $
    do modifyMaxSuccess (const 500) $ it "prop_doesNotIncrement" $ property doesNotIncrement
       modifyMaxSuccess (const 500) $ it "prop_doesIncrement" $ property doesIncrement
