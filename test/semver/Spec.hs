import Test.QuickCheck
import Test.Hspec
import Data.List
import Semver (SemanticVersion(..), lexLeq)

instance Arbitrary SemanticVersion where
  arbitrary = SemanticVersion <$> arbitrary

prop_eq_reflex :: SemanticVersion -> Property
prop_eq_reflex = reflexivity (==)

prop_eq_trans :: SemanticVersion -> SemanticVersion -> SemanticVersion -> Property
prop_eq_trans a b c =
  let x = sort [a,b,c]
  in transitivity (==) (head x) (head(tail x)) (head(tail(tail x)))

prop_lexLeq_reflex :: SemanticVersion -> Property
prop_lexLeq_reflex = reflexivity lexLeq

prop_lexLeq_trans :: SemanticVersion -> SemanticVersion -> SemanticVersion -> Property
prop_lexLeq_trans = transitivity lexLeq

prop_lexLeq_antisym :: SemanticVersion -> SemanticVersion -> Property
prop_lexLeq_antisym a b = (lexLeq a b && lexLeq b a) ==> (a == b)

reflexivity :: (a -> a -> Bool) -> a -> Property
reflexivity f a = f a a .&&. True

transitivity :: (a -> a -> Bool) -> a -> a -> a -> Property
transitivity f a b c = (f a b && f b c) ==> f a c

main :: IO ()
main = hspec $ do
  describe "Eq" $
    do it "eq_reflex" $ property prop_eq_reflex
       it "eq_trans" $ property prop_eq_trans
  describe "Ord" $
    do it "prop_lexLeq_reflex" $ property prop_lexLeq_reflex
       it "prop_lexLeq_trans" $ property prop_lexLeq_trans
       it "prop_lexLeq_antisym" $ property prop_lexLeq_antisym
