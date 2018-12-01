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
invalidExpression = error "not impl"

-- 12 + App (Abs "x" (Var "x")) 6
-- 12 + App (FunVal "x" (Var "x")) 6
-- 12 + (FunVal "x" (Var "x")) where 6 is inserted into the environment as "x" at application evaluation
-- 12 + (Var "x") after the evaluation 
-- 12 + 6
-- 18
validExpression = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- Reference parser  a.k.a eval0
evalRef :: Env -> Exp -> Value
evalRef env (Lit i) = IntVal i
evalRef env (Var n) = fromJust (M.lookup n env) -- Carries an unhandled effect
evalRef env (Plus exp1 exp2) =
  let IntVal v1 = evalRef env exp1
      IntVal v2 = evalRef env exp2
   in IntVal (v1 + v2)
evalRef env (Abs nm exp) = FunVal env nm exp -- Means function application (not evaluation) ((\x -> x)(6)) 
evalRef env (App exp1 exp2) =
  let v1 = evalRef env exp1
      v2 = evalRef env exp2
   in case v1
--      _ -> Unhandled possibilities
        -- function application binds the value to the environment before evaluate.
            of
        FunVal _ nm body -> evalRef (M.insert nm v2 env) body
