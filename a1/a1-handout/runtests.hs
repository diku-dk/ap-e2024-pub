import qualified APL.AST_Tests
import qualified APL.Eval_Tests
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "APL"
      [ APL.AST_Tests.tests,
        APL.Eval_Tests.tests
      ]
