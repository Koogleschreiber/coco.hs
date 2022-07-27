import SemanticVersionSpec (semanticVersionSuite)
import Test.Hspec
import ConventionalCommitSpec (conventionalCommitSpec)

main :: IO ()
main = hspec $ do
      semanticVersionSuite
      conventionalCommitSpec

