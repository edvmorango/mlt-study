-- Monad Transformers - Step by Step
module Paper where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Maybe

-- Language
type Name = String

type Env = M.Map Name Value

data Exp
  = Lit Integer
  | Var Name
  | Plus Exp
         Exp
  | Abs Name
        Exp
  | App Exp
        Exp
  deriving (Show)

data Value
  = IntVal Integer
  | FunVal Env
           Name
           Exp
  deriving (Show)

-- Expressions
invalidExpression = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))

invalidExpressionPlus = Plus (Lit 1) (Abs "x" (Var "x"))

invalidExpressionApp = App (Lit 6) (Abs "x" (Var "x"))

-- 12 + App (Abs "x" (Var "x")) 6
-- 12 + App (FunVal "x" (Var "x")) 6
-- 12 + (FunVal "x" (Var "x")) where 6 is inserted into the environment as "x" at application evaluation
-- 12 + (Var "x") after the evaluation 
-- 12 + 6
-- 18
validExpression = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- Reference parser  a.k.a. eval0
evalRef :: Env -> Exp -> Value
evalRef env (Lit i) = IntVal i
evalRef env (Var n) = fromJust (M.lookup n env) -- Carries an unhandled effect
evalRef env (Plus exp1 exp2) =
  let IntVal v1 = evalRef env exp1
      IntVal v2 = evalRef env exp2 -- Error when expressions aren't IntVal
   in IntVal (v1 + v2)
evalRef env (Abs nm exp) = FunVal env nm exp -- Means function application (not evaluation) ((\x -> x)(6)) 
evalRef env (App exp1 exp2) =
  let v1 = evalRef env exp1
      v2 = evalRef env exp2
   in case v1
        -- function application binds the value to the environment before evaluate.
            of
        FunVal _ nm body -> evalRef (M.insert nm v2 env) body
        _ -> error "unhandled effect"

-- Identity Parser a.k.a. eval1
type EvalId a = Identity a

runEvalId :: EvalId a -> a
runEvalId a = runIdentity a

evalId :: Env -> Exp -> EvalId Value
evalId env (Lit i) = return $ IntVal i
evalId env (Var n) = return $ fromJust (M.lookup n env) -- M.lookup signature is binded to Maybe not to (Monad m) => ...
evalId env (Plus e1 e2) = do
  IntVal v1 <- evalId env e1
  IntVal v2 <- evalId env e2
  return $ IntVal (v1 + v2)
evalId env (Abs nm exp) = return $ FunVal env nm exp
evalId env (App e1 e2) = do
  v1 <- evalId env e1
  v2 <- evalId env e2
  case v1 of
    FunVal _ nm body -> evalId (M.insert nm v2 env) body

-- ErrorT parser a.k.a. eval2
-- where Error e (m :: * -> * ) a 
-- e :: Error
-- m :: Innermost monad
-- a :: kind of innermost monad
-- Error String Identity a
-- Error    e      m     a
type EvalErrorT a = ErrorT String Identity a

runEvalErrorT :: EvalErrorT a -> Either String a
runEvalErrorT ev = runIdentity $ runErrorT ev

maybeToEither :: Maybe a -> Either String a
maybeToEither (Just a) = Right a
maybeToEither _ = Left "Empty value"

evalErrorT :: Env -> Exp -> EvalErrorT Value
evalErrorT env (Lit i) = return $ IntVal i
evalErrorT env (Var n) = do
  let a = (M.lookup n env)
   in case a of
        (Just v) -> return v
        Nothing -> throwError $ ("Couldn't find " ++ n)
--evalErrorT env (Plus e1 e2) = do
--  v1 <- evalErrorT env e1
--  v2 <- evalErrorT env e2
--  case (v1, v2) of
--    (IntVal v1', IntVal v2') -> return $ IntVal (v1' + v2')
--    _ -> throwError "Invalid expression"
evalErrorT env (Plus e1 e2) = do
  IntVal v1 <- evalErrorT env e1
  IntVal v2 <- evalErrorT env e2
  return $ IntVal (v1 + v2)
evalErrorT env (Abs nm exp) = return $ FunVal env nm exp
evalErrorT env (App e1 e2)
        -- v1 <- evalErrorT env e1
 = do
  FunVal _ nm body <- evalErrorT env e1
  v2 <- evalErrorT env e2
  evalErrorT (M.insert nm v2 env) body

--  case v1 of
--    FunVal _ nm body -> evalErrorT (M.insert nm v2 env) body
--   _ -> throwError $ (show e1) ++ " is not a function"
-- ReaderT embedding ErrorT a.k.a. eval3
-- where a provides kind to Identity
type EvalReaderErrorT a = ReaderT Env (ErrorT String Identity) a

runEvalReaderErrorT :: Env -> EvalReaderErrorT a -> Either String a
runEvalReaderErrorT env comp = runIdentity $ runErrorT $ runReaderT comp env

evalReaderErrorT :: Exp -> EvalReaderErrorT Value
evalReaderErrorT (Lit i) = return $ IntVal i
evalReaderErrorT (Var n) = do
  env <- ask
  case (M.lookup n env) of
    (Just a) -> return a
    _ -> throwError $ ("Couldn't find `" ++ n ++ "`")
-- Without customized error message
evalReaderErrorT (Plus e1 e2) = do
  IntVal v1 <- evalReaderErrorT e1
  IntVal v2 <- evalReaderErrorT e2
  return $ IntVal (v1 + v2)
evalReaderErrorT (Abs nm exp) = do
  env <- ask
  return $ FunVal env nm exp
evalReaderErrorT (App e1 e2) = do
  v1 <- evalReaderErrorT e1
  v2 <- evalReaderErrorT e2
  case v1 of
    (FunVal env nm body) ->
      local (const (M.insert nm v2 env)) (evalReaderErrorT body)
    _ -> throwError $ "`" ++ (show v1) ++ "` is not a function"

-- ReaderT embedding ErrorT and StateT a.k.a eval4
type EvalREST a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEvalREST :: Env -> Integer -> EvalREST a -> (Either String a, Integer)
runEvalREST env state comp =
  runIdentity $ runStateT (runErrorT (runReaderT comp env)) state

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

evalREST :: Exp -> EvalREST Value
evalREST (Lit i) = do
  _ <- tick
  return $ IntVal i
evalREST (Var n) = do
  tick
  env <- ask
  case (M.lookup n env) of
    (Just a) -> return a
    _ -> throwError $ ("Couldn't find `" ++ n ++ "`")
evalREST (Plus e1 e2) = do
  _ <- tick
  IntVal v1 <- evalREST e1
  IntVal v2 <- evalREST e2
  return $ IntVal (v1 + v2)
evalREST (Abs nm exp) = do
  _ <- tick
  env <- ask
  return $ FunVal env nm exp
evalREST (App e1 e2) = do
  _ <- tick
  v1 <- evalREST e1
  v2 <- evalREST e2
  case v1 of
    FunVal env nm body -> local (const (M.insert nm v2 env)) (evalREST body)
    _ -> throwError $ "`" ++ (show e1) ++ "` is not a function"

-- ReaderT + ErrorT + StateT + WriterT a.k.a eval5
type EvalRESWT a
   = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a

runEvalRESWT ::
     Env -> Integer -> EvalRESWT a -> ((Either String a, [String]), Integer)
runEvalRESWT env state comp =
  runIdentity $ runStateT (runWriterT (runErrorT (runReaderT comp env))) state

evalRESWT :: Exp -> EvalRESWT Value
evalRESWT (Lit i) = do
  _ <- tick
  return $ IntVal i
evalRESWT (Var n) = do
  _ <- tick
  _ <- tell [n]
  env <- ask
  case (M.lookup n env) of
    (Just a) -> return a
    _ -> throwError $ "Couldn't find " ++ n
evalRESWT (Plus e1 e2) = do
  _ <- tick
  IntVal v1 <- evalRESWT e1
  IntVal v2 <- evalRESWT e2
  return $ IntVal (v1 + v2)
evalRESWT (Abs nm exp) = do
  _ <- tick
  env <- ask
  return $ FunVal env nm exp
evalRESWT (App e1 e2) = do
  _ <- tick
  v1 <- evalRESWT e1
  v2 <- evalRESWT e2
  case v1 of
    FunVal env nm body -> local (const (M.insert nm v2 env)) (evalRESWT body)
    _ -> throwError $ "`" ++ (show e1) ++ "` is not a function"

-- ReaderT + ErrorT + StateT + WriterT with IO as innermost  a.k.a eval6
type EvalIO a
   = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

runEvalIO ::
     Env -> Integer -> EvalIO a -> IO ((Either String a, [String]), Integer)
runEvalIO env state comp =
  runStateT (runWriterT (runErrorT (runReaderT comp env))) state

evalIO :: Exp -> EvalIO Value
evalIO (Lit i) = do
  _ <- tick
  _ <- liftIO $ print i
  return $ IntVal i
evalIO (Var n) = do
  _ <- tick
  _ <- tell [n]
  env <- ask
  case (M.lookup n env) of
    (Just a) -> return a
    _ -> throwError $ "Couldn't find " ++ n
evalIO (Plus e1 e2) = do
  _ <- tick
  IntVal v1 <- evalIO e1
  IntVal v2 <- evalIO e2
  return $ IntVal (v1 + v2)
evalIO (Abs nm exp) = do
  _ <- tick
  env <- ask
  return $ FunVal env nm exp
evalIO (App e1 e2) = do
  _ <- tick
  v1 <- evalIO e1
  v2 <- evalIO e2
  case v1 of
    FunVal env nm body -> local (const (M.insert nm v2 env)) (evalIO body)
    _ -> throwError $ "`" ++ (show e1) ++ "` is not a function"
