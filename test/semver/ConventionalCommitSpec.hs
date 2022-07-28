module ConventionalCommitSpec where
import Test.QuickCheck
import Test.Hspec
import Data.List
import RelationLaws (implies, transitivity, reflexivity)
import ConventionalCommit (ConventionalCommit(..), ChangeType(..), readChangeType)
import Test.Hspec.QuickCheck (modifyMaxSuccess)

genChangeType :: Gen ChangeType
genChangeType = do
    i <- arbitrary
    n <- getPrintableString <$> arbitrary
    return (ChangeType (i, n))

instance Arbitrary ChangeType where
  arbitrary = genChangeType

prop_eq_reflex :: ChangeType -> Bool
prop_eq_reflex = reflexivity (==)

prop_eq_trans :: ChangeType -> ChangeType -> ChangeType -> Bool
prop_eq_trans = transitivity (==)

prop_unequal :: ChangeType -> NonEmptyList Char -> Bool
prop_unequal (ChangeType (i, n)) s = ChangeType (i, n) /= ChangeType (i, n++st)
     where st = getNonEmpty s

prop_num_irrelevant :: ChangeType -> Int -> Bool
prop_num_irrelevant (ChangeType (i, n)) x = ChangeType (i, n) == ChangeType (x, n)

prop_read_inverse :: ChangeType  -> Bool
prop_read_inverse s = (readChangeType . show) s == s

conventionalCommitSpec = do
  describe "ChangeType Eq" $
    do modifyMaxSuccess (const 500) $ it "prop_eq_reflex" $ property prop_eq_reflex
       modifyMaxSuccess (const 500) $ it "prop_eq_trans" $ property prop_eq_trans
       modifyMaxSuccess (const 500) $ it "prop_eq_unequal" $ property prop_unequal
       modifyMaxSuccess (const 500) $ it "prop_num_irrelevant" $ property prop_num_irrelevant
  describe "ChangeType Show" $
    do modifyMaxSuccess (const 500) $ it "prop_eq_reflex" $ property prop_read_inverse
