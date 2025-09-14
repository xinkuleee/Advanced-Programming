module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "ForLoop" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
          @?= Right (ValInt 45),
      --
      testCase "Forloop(sub)" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Sub (Var "p") (Var "i")))
          @?= Right (ValInt (-45)),
      --
      testCase "Forloop(mul)" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 1) ("i", CstInt 4) (Mul (Var "p") (Add (Var "i") (CstInt 1))))
          @?= Right (ValInt 24),
      --
      testCase "Forloop(div)" $
         eval
          envEmpty
          (ForLoop ("p", CstInt 24) ("i", CstInt 4) (Div (Var "p") (Add (Var "i") (CstInt 1))))
          @?= Right (ValInt 1),
      --
      testCase "Forloop(div0)" $
         eval
          envEmpty
          (ForLoop ("p", CstInt 24) ("i", CstInt 4) (Div (Var "p") (Var "i")))
          @?= Left "Division by zero",
      testCase "Forloop(pow)" $
        eval
          envEmpty
            (ForLoop ("p", CstInt 2) ("i", CstInt 3) (Pow (Var "p") (Add (Var "i") (CstInt 1))))
          @?= Right (ValInt 64),
      --
      testCase "Forloop(let)" $
        eval
          envEmpty
          (ForLoop ("p", CstInt 0) ("i", CstInt 4) (Let "tmp" (Mul (Var "i") (CstInt 2)) (Add (Var "p") (Var "tmp"))))
          @?= Right (ValInt 12),
      --
      testCase "Lambda1" $
         eval
          envEmpty
          (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right (ValFun [("x",ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
      testCase "Lambda2" $
         eval
          envEmpty
          (Let "x" (CstInt 2) (Lambda "x" (Add (Var "x") (CstInt 1))))
          @?= Right (ValFun [("x",ValInt 2)] "x" (Add (Var "x") (CstInt 1))),
      --
      testCase "Apply(let)" $
         eval
          envEmpty
          (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= Right (ValInt 5),
      --
      testCase "Apply(Lambda)" $
        eval
          envEmpty
          (Apply (Lambda "x" (Mul (Var "x") (CstInt 2))) (Add (CstInt 3) (CstInt 4)))
          @?= Right (ValInt 14),
      --
      testCase "Apply(LetandMul)" $
        eval
          envEmpty
          (Apply
            (Let "a" (CstInt 10) (Lambda "b" (Add (Var "a") (Var "b"))))
            (Mul (CstInt 2) (CstInt 3)))
          @?= Right (ValInt 16),
      --
      testCase "Apply evaluates function first: error in function" $
        eval
          envEmpty 
          ( Apply (Var "f_missing") (CstInt 1) )
          @?= Left "Unknown variable: f_missing",
      --
      testCase "Apply evaluates argument second: error in argument" $
        eval
            envEmpty 
            (Apply (Lambda "x" (CstInt 1)) (Var "a_missing") )
            @?= Left "Unknown variable: a_missing",
      --
      testCase "Apply(lambda_apply)" $
        eval
            envEmpty 
            (Apply (Lambda "x" (Apply (Var "x") (Var "x"))) (Lambda "x" (Apply (Var "x") (Var "x"))))
            @?= Left "err",
      --
      testCase "TryCatch (no error)" $
        eval
          envEmpty
          ( TryCatch
                (Add (CstInt 2) (CstInt 3))
                (CstInt 42)
          )
          @?= Right (ValInt 5),
      --
      testCase "TryCatch (with error)" $
        eval
          envEmpty
          ( TryCatch
              (Div (CstInt 2) (CstInt 0))
               (CstInt 42)
          )
          @?= Right (ValInt 42),
      --
      testCase "TryCatch(trycatch)" $
        eval
          envEmpty
          (TryCatch (TryCatch(CstInt 0) (CstInt 1)) (CstInt 1))
          @?= Right (ValInt 0),
      --
      testCase "TryCatch(trycatch2)" $
        eval
          envEmpty
          (TryCatch (Div (CstInt 2) (CstInt 0)) (TryCatch(CstInt 3) (CstInt 4)))
          @?= Right (ValInt 3)
      --
    ]
