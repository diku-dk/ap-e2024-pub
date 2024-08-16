import qualified APL.Interp_Tests
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain APL.Interp_Tests.tests
