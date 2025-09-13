module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | ForLoop (VName, Exp) (VName, Exp) Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  -- TODO: add cases
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt n)     = show n
printExp (CstBool True) = "true"
printExp (CstBool False)= "false"
printExp (Var v)        = v
printExp (Add e1 e2)    = "(" ++ printExp e1 ++ " + " ++ printExp e2 ++ ")"
printExp (Sub e1 e2)    = "(" ++ printExp e1 ++ " - " ++ printExp e2 ++ ")"
printExp (Mul e1 e2)    = "(" ++ printExp e1 ++ " * " ++ printExp e2 ++ ")"
printExp (Div e1 e2)    = "(" ++ printExp e1 ++ " / " ++ printExp e2 ++ ")"
printExp (Pow e1 e2)    = "(" ++ printExp e1 ++ " ** " ++ printExp e2 ++ ")"
printExp (Eql e1 e2)    = "(" ++ printExp e1 ++ " == " ++ printExp e2 ++ ")"
printExp (If c t e)     = "(if " ++ printExp c ++ " then " ++ printExp t ++ " else " ++ printExp e ++ ")"
printExp (Let v e1 e2)  = "(let " ++ v ++ " = " ++ printExp e1 ++ " in " ++ printExp e2 ++ ")"
printExp (ForLoop (p,e1) (i,e2) body) =
  "(loop " ++ p ++ " = " ++ printExp e1 ++
  " for " ++ i ++ " < " ++ printExp e2 ++
  " do " ++ printExp body ++ ")"
printExp (Lambda v body) =
  "(\\" ++ v ++ " -> " ++ printExp body ++ ")"
printExp (Apply f x) =
  let fStr = case f of
               Var _     -> printExp f
               CstInt _  -> printExp f
               CstBool _ -> printExp f
               Apply _ _ -> printExp f
               _         -> "(" ++ printExp f ++ ")"
      xStr = case x of
               Var _     -> printExp x
               CstInt _  -> printExp x
               CstBool _ -> printExp x
               _         -> "(" ++ printExp x ++ ")"
   in "(" ++ fStr ++ " " ++ xStr ++ ")"
printExp (TryCatch e1 e2) =
  "(try " ++ printExp e1 ++ " catch " ++ printExp e2 ++ ")"
