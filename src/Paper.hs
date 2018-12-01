-- Monad Transformers - Step by Step
module Paper where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Maybe

-- Interpreter
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
