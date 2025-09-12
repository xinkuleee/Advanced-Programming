module APL.Eval_Tests (tests) where

import APL.AST (Exp (..), printExp)
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
          ( ForLoop
              ("p", CstInt 0)
              ("i", CstInt 10)
              (Add (Var "p") (Var "i"))
          )
          @?= Right (ValInt 45),
      --
      testCase "Lambda" $
        eval
          envEmpty
          ( Let
              "x" (CstInt 2)
              ( Lambda "y" (Add (Var "x") (Var "y")))
          )
          @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),
      --
        testCase "Function" $
          eval
            envEmpty
            ( Apply (Let "x" (CstInt 2)
                (Lambda "y" (Add (Var "x") (Var "y"))))
                (CstInt 3)
            )
            @?= Right (ValInt 5),
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
        testCase "printEXP (print Int)" $
          printExp (CstInt 42) @?= "42",
      --
        testCase "printEXP (print Bool)" $
          printExp (CstBool True) @?= "true",
      --
        testCase "printEXP (print Add)" $
          printExp (Add (CstInt 2) (CstInt 3))
            @?= "(2 + 3)",
      --
        testCase "printEXP (print If)" $
          printExp (If (CstBool True) (CstInt 2) (CstInt 3))
            @?= "(if true then 2 else 3)",
      --
        testCase "printEXP (print Let)" $
          printExp (Let "x" (CstInt 2) (Add (Var "x") (CstInt 3)))
            @?= "(let x = 2 in (x + 3))",
      --
        testCase "printEXP (print Lambda)" $
          printExp (Lambda "x" (Add (Var "x") (CstInt 3)))
            @?= "\\x -> (x + 3)",
      --
        testCase "printEXP (print Apply)" $
          printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 3))) (CstInt 2))
            @?= "(\\x -> (x + 3)) 2",
      --
        testCase "printEXP (print try-catch)" $
          printExp (TryCatch (Div (CstInt 2) (CstInt 0)) (CstInt 42))
            @?= "(try (2 / 0) catch 42)",
      --
        testCase "printEXP (print forloop)" $
          printExp (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
            @?= "(loop p = 0 for i < 10 do (p + i))"
      --
          -- TODO - add more
    ]
