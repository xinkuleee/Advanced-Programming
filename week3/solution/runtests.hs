import qualified APL.Parser_Tests
import Test.Tasty (defaultMain, localOption, mkTimeout)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) $ APL.Parser_Tests.tests
