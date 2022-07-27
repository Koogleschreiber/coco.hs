module ConventionalCommitSpec where
import Test.QuickCheck
import Test.Hspec
import Data.List
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

prop_read_inverse :: ChangeType  -> Bool
prop_read_inverse s = (readChangeType . show) s == s

reflexivity :: (a -> a -> Bool) -> a -> Bool
reflexivity f a = f a a

conventionalCommitSpec = do
  describe "ChangeType Eq" $
    do modifyMaxSuccess (const 5000) $ it "prop_eq_reflex" $ property prop_eq_reflex
       modifyMaxSuccess (const 5000) $ it "prop_eq_reflex" $ property prop_read_inverse
