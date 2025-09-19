module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String


type Env = [String]
newtype CheckM a = CheckM (Env -> Either Error a) -- TODO - give this a proper definition.

instance Functor CheckM where
  fmap f (CheckM m) = CheckM $ \env -> fmap f (m env)

instance Applicative CheckM where
  pure x = CheckM $ \_ -> Right x
  (CheckM f) <*> (CheckM x) = CheckM $ \env -> f env <*> x env

instance Monad CheckM where
  (CheckM m) >>= f = CheckM $ \env ->
    case m env of
      Left err -> Left err
      Right v  -> let (CheckM m2) = f v
                  in m2 env

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

failure :: Error -> CheckM a
failure e = CheckM $ \_ -> Left e

check :: Exp -> CheckM ()
check (CstInt _)  = pure ()
check (CstBool _) = pure ()
check (Var v) = do
  env <- askEnv
  if v `elem` env
    then pure ()
    else failure $ "Variable not in scope: " ++ v
check (Add e1 e2) = check e1 >> check e2
check (Sub e1 e2) = check e1 >> check e2
check (Mul e1 e2) = check e1 >> check e2
check (Div e1 e2) = check e1 >> check e2
check (Pow e1 e2) = check e1 >> check e2
check (Eql e1 e2) = check e1 >> check e2
check (If c e1 e2) = check c >> check e1 >> check e2
check (Let v e1 e2) = do
  check e1
  localEnv (v:) (check e2)  -- v 在 e2 中是可见的
check (Lambda v body) =
  localEnv (v:) (check body)
check (Apply e1 e2) = check e1 >> check e2
check (Print _ e)   = check e
check (TryCatch e1 e2) = check e1 >> check e2
check (ForLoop (loopparam, initExp) (iv, bound) body) = do
  check initExp
  check bound
  localEnv (\env -> loopparam : iv : env) (check body)

check (KvPut k v) = check k >> check v
check (KvGet k)   = check k



checkExp :: Exp -> Maybe Error
checkExp e =
  case runCheck (check e) [] of
    Left err -> Just err
    Right _  -> Nothing

runCheck :: CheckM a -> Env -> Either Error a
runCheck (CheckM m) env = m env
