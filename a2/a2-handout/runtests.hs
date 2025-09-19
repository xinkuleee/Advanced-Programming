import qualified APL.Check_Tests
import qualified APL.Eval_Tests
import Test.Tasty (defaultMain, localOption, mkTimeout, testGroup)

main :: IO ()
main =
  defaultMain $
    localOption (mkTimeout 1000000) $
      testGroup
        "APL"
        [ APL.Eval_Tests.tests,
          APL.Check_Tests.tests
        ]
