module ConventionalCommitSpec where
import Test.QuickCheck
import Test.Hspec
import Data.List
import ConventionalCommit (ConventionalCommit(..), ChangeType(..))
import Test.Hspec.QuickCheck (modifyMaxSuccess)

genChangeType :: Gen ChangeType
genChangeType = do
    x <- arbitrary
    xs <- arbitrary
    return (ChangeType (x, xs))

instance Arbitrary ChangeType where
  arbitrary = genChangeType

prop_eq_reflex :: ChangeType -> Bool
prop_eq_reflex = reflexivity (==)

reflexivity :: (a -> a -> Bool) -> a -> Bool
reflexivity f a = f a a

conventionalCommitSpec = do
  describe "ConventionalCommit Eq" $
    do modifyMaxSuccess (const 5000) $ it "prop_eq_reflex" $ property prop_eq_reflex
