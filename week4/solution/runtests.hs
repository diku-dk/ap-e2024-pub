import qualified APL.Eval_Tests
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain APL.Eval_Tests.tests
