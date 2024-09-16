import qualified APL.Check_Tests
import qualified APL.Eval_Tests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "APL"
      [ APL.Eval_Tests.tests,
        APL.Check_Tests.tests
      ]
