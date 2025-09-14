module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      testCase "CstInt" $
        printExp (CstInt 10)
        @?= "10",
      --
      testCase "CstBoolTrue" $
        printExp (CstBool True)
        @?= "true",
      --
      testCase "CstBoolFalse" $
        printExp (CstBool False)
        @?= "false",
      --
      testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 3))
        @?= "(2 + 3)",
      --
      testCase "Sub" $
        printExp (Sub (CstInt 3) (CstInt 2))
        @?= "(3 - 2)",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 3) (CstInt 2))
        @?= "(3 * 2)",
      --
      testCase "Div" $
        printExp (Div (CstInt 3) (CstInt 2))
        @?= "(3 / 2)",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 3) (CstInt 2))
        @?= "(3 ** 2)",
      --
      testCase "Eql" $
        printExp (Eql (CstInt 5) (CstInt 5))
        @?= "(5 == 5)",
      --
      testCase "If" $
        printExp (If (Eql  (CstInt 5) (CstInt 5)) (CstInt 1) (CstInt 0))
        @?= "(if (5 == 5) then 1 else 0)",
      --
      testCase "Let" $
        printExp (Let "x" (CstInt 1) (Add (Var "x") (CstInt 2)))
        @?= "(let x = 1 in (x + 2))",
      --
      testCase "Forloop" $
        printExp (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
        @?= "(loop p = 0 for i < 10 do (p + i))",
      --
      testCase "Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 1)))
        @?= "(\\x -> (x + 1))",
      --
      testCase "Apply(constant)" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5))
        @?= "(((\\x -> (x + 1))) 5)",
      --
      testCase "Apply(exp)" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (Add (CstInt 2) (CstInt 3)))
        @?= "(((\\x -> (x + 1))) ((2 + 3)))",
      --
      testCase "Apply(nest)" $
        printExp (Apply (Apply (Lambda "x" (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 2)) (CstInt 3))
        @?= "((((\\x -> (\\y -> (x + y)))) 2) 3)",
      --
      testCase "TryCatch" $
        printExp (TryCatch (Div (CstInt 1) (CstInt 0)) (CstInt 999))
          @?= "(try (1 / 0) catch 999)"
      --
    ]
