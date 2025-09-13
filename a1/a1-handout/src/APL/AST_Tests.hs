module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
        testCase "printExp (print Int)" $
          printExp (CstInt 42) @?= "42",
      --
        testCase "printExp (print Bool)" $
          printExp (CstBool True) @?= "true",
      --
        testCase "printExp (print Add)" $
          printExp (Add (CstInt 2) (CstInt 3))
            @?= "(2 + 3)",
      --
        testCase "printExp (print If)" $
          printExp (If (CstBool True) (CstInt 2) (CstInt 3))
            @?= "(if true then 2 else 3)",
      --
        testCase "printExp (print Let)" $
          printExp (Let "x" (CstInt 2) (Add (Var "x") (CstInt 3)))
            @?= "(let x = 2 in (x + 3))",
      --
        testCase "printExp (print Lambda)" $
          printExp (Lambda "x" (Add (Var "x") (CstInt 3)))
            @?= "\\x -> (x + 3)",
      --
        testCase "printExp (print Apply)" $
          printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 3))) (CstInt 2))
            @?= "(\\x -> (x + 3)) 2",
      --
        testCase "printExp (print try-catch)" $
          printExp (TryCatch (Div (CstInt 2) (CstInt 0)) (CstInt 42))
            @?= "(try (2 / 0) catch 42)",
      --
        testCase "printExp (print forloop)" $
          printExp (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var "i")))
            @?= "(loop p = 0 for i < 10 do (p + i))"
      --
    ]
