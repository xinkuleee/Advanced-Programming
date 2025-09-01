import qualified APL.Eval_Tests
import Test.Tasty (defaultMain, localOption, mkTimeout)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) APL.Eval_Tests.tests
