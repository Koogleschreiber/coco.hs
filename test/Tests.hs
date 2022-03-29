import Test.HUnit
import Git (splitIntoCommitMessages)

emptySplit = TestCase (assertEqual "emptySplit" (splitIntoCommitMessages []) [""] )
twoLinesSplit = TestCase (assertEqual "twoLinesSplit" (splitIntoCommitMessages "fizz\nbuzz") ["fizz", "buzz"] )

tests :: Test
tests = TestList [emptySplit, twoLinesSplit]

main :: IO Counts
main = do
  runTestTT tests
