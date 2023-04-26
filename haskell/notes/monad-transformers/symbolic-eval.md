# Symbolic Evaluator

**Source**: **Monad Transformers Step by Step** by Martin GrabmÜller

```haskell
module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Expression = Literal Integer
                | Variable Name
                | Add Expression Expression
                | Lambda Name Expression
                | Apply Expression Expression
                deriving (Show)

data Value = IntVal Integer
           | FunVal Environment Name Expression
           deriving (Show)

type Environment = Map.Map Name Value

eval1 :: Environment -> Expression -> Value
eval1 env (Literal i) = IntVal i
eval1 env (Variable n) = fromJust (Map.lookup n env)
eval1 env (Add x y) = let IntVal i = eval1 env x
                          IntVal j = eval1 env y
                      in IntVal (i + j)

eval1 env (Lambda n e) = FunVal env n e
eval1 env (Apply e1 e2) = let x = eval1 env e1
                              y = eval1 env e2
                          in case x of
                            FunVal env' n body -> eval1 (Map.insert n y env') body

{- 
  12 + ((λx -> x) (4 + 2))
  example = Literal 12 `add` (Apply (Lambda "x" (Variable "x")) (Literal 4 `add` Literal 2))
  eval1 Map.empty example -> IntVal 18 
-}

-- Monad Identity

type Eval2 a = Identity a

runEval2 :: Eval1 a -> a
runEval2 ev = runIdentity ev

eval2 :: Environment -> Expression -> Eval2 Value
eval2 env (Literal i) = return $ IntVal i
eval2 env (Variable n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2 env (Add x y) = do IntVal <- eval2 env x
                         IntVal <- eval2 env y
                         return $ IntVal (x + y)
eval2 env (Lambda n e) = return $ FunVal env n e
eval2 env (Apply e1 e2) = do x <- eval2 env e1
                             y <- eval2 env e2
                             case x of
                                FunVal env' n body ->
                                    eval2 (Map.insert n y env') body
```
