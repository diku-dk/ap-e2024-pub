import qualified APL.Tests
import Test.Tasty (defaultMain)
import Test.Tasty.QuickCheck (testProperties)

main :: IO ()
main = defaultMain (testProperties "APL properties" APL.Tests.properties)
