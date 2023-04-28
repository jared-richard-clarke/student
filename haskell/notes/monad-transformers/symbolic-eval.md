# Symbolic Evaluator

**Source**: **Monad Transformers Step by Step** by Martin GrabmÜller

```haskell
module Transformers where

-- Control.Monad.Error deprecated in newer versions of Haskell
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as Map
import Data.Maybe (fromJust)

type Name = String

data Expression
  = Literal Integer
  | Variable Name
  | Add Expression Expression
  | Lambda Name Expression
  | Apply Expression Expression
  deriving (Show)

data Value
  = IntVal Integer
  | FunVal Environment Name Expression
  deriving (Show)

type Environment = Map.Map Name Value

eval1 :: Environment -> Expression -> Value
eval1 env (Literal i) = IntVal i
eval1 env (Variable n) = fromJust (Map.lookup n env)
eval1 env (Add x y) =
  let IntVal i = eval1 env x
      IntVal j = eval1 env y
   in IntVal (i + j)
eval1 env (Lambda n e) = FunVal env n e
eval1 env (Apply e1 e2) =
  let x = eval1 env e1
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

runEval2 :: Eval2 a -> a
runEval2 = runIdentity

eval2 :: Environment -> Expression -> Eval2 Value
eval2 env (Literal i) = return $ IntVal i
eval2 env (Variable n) = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval2 env (Add x y) = do
  IntVal i <- eval2 env x
  IntVal j <- eval2 env y
  return $ IntVal (i + j)
eval2 env (Lambda n e) = return $ FunVal env n e
eval2 env (Apply e1 e2) = do
  x <- eval2 env e1
  y <- eval2 env e2
  case x of
    FunVal env' n body ->
      eval2 (Map.insert n y env') body

-- Monad Identity + Error

type Eval3 a = ErrorT String Identity a

runEval3 :: Eval3 a -> Either String a
runEval3 ev = runIdentity (runErrorT ev)

eval3 :: Environment -> Expression -> Eval3 Value
eval3 env (Literal i) = return $ IntVal i
eval3 env (Variable n) = case Map.lookup n env of
  Nothing -> throwError ("unbound variable: " ++ n)
  Just x -> return x
eval3 env (Add x y) = do
  x' <- eval3 env x
  y' <- eval3 env y
  case (x', y') of
    (IntVal i, IntVal j) ->
      return $ IntVal (i + j)
    _ -> throwError "type error in addition"
eval3 env (Lambda n e) = return $ FunVal env n e
eval3 env (Apply e1 e2) = do
  x <- eval3 env e1
  y <- eval3 env e2
  case x of
    FunVal env' n body ->
      eval3 (Map.insert n y env') body
    _ -> throwError "type error in application"

-- Monad Identity + Error + Reader

type Eval4 a = ReaderT Environment (ErrorT String Identity) a

runEval4 :: Environment -> Eval4 a -> Either String a
runEval4 env ev = runIdentity (runErrorT (runReaderT ev env))

eval4 :: Expression -> Eval4 Value
eval4 (Literal i) = return $ IntVal i
eval4 (Variable n) = do
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just x -> return x
eval4 (Add x y) = do
  x' <- eval4 x
  y' <- eval4 y
  case (x', y') of
    (IntVal i, IntVal j) ->
      return $ IntVal (i + j)
    _ -> throwError "type error in addition"
eval4 (Lambda n e) = do
  env <- ask
  return $ FunVal env n e
eval4 (Apply e1 e2) = do
  x <- eval4 e1
  y <- eval4 e2
  case x of
    FunVal env' n body ->
      local (const (Map.insert n y env')) (eval4 body)
    _ -> throwError "type error in application"

-- Monad Identity + Error + Reader + State

type Eval5 a = ReaderT Environment (ErrorT String (StateT Integer Identity)) a

runEval5 :: Environment -> Integer -> Eval5 a -> (Either String a, Integer)
runEval5 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval5 :: Expression -> Eval5 Value
eval5 (Literal i) = do
  tick
  return $ IntVal i
eval5 (Variable n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval5 (Add x y) = do
  tick
  x' <- eval5 x
  y' <- eval5 y
  case (x', y') of
    (IntVal i, IntVal j) ->
      return $ IntVal (i + j)
    _ -> throwError "type error in addition"
eval5 (Lambda n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval5 (Apply e1 e2) = do
  tick
  x <- eval5 e1
  y <- eval5 e2
  case x of
    FunVal env' n body ->
      local (const (Map.insert n y env')) (eval5 body)
    _ -> throwError "type error in application"

-- Monad Identity + Error + Reader + State + Writer

type Eval6 a = ReaderT Environment (ErrorT String (WriterT [String] (StateT Integer Identity))) a

runEval6 :: Environment -> Integer -> Eval6 a -> ((Either String a, [String]), Integer)
runEval6 env st ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

eval6 :: Expression -> Eval6 Value
eval6 (Literal i) = do
  tick
  return $ IntVal i
eval6 (Variable n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval6 (Add x y) = do
  tick
  x' <- eval6 x
  y' <- eval6 y
  case (x', y') of
    (IntVal i, IntVal j) -> return $ IntVal (i + j)
    _ -> throwError "type error in addition"
eval6 (Lambda n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval6 (Apply e1 e2) = do
  tick
  x <- eval6 e1
  y <- eval6 e2
  case x of
    FunVal env' n body ->
      local (const (Map.insert n y env')) (eval6 body)
    _ -> throwError "type error in application"

-- Monad Identity + Error + Reader + State + Writer + IO

type Eval7 a = ReaderT Environment (ErrorT String (WriterT [String] (StateT Integer IO))) a

eval7 :: Expression -> Eval7 Value
eval7 (Literal i) = do
  tick
  liftIO $ print i
  return $ IntVal i
eval7 (Variable n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Nothing -> throwError ("unbound variable: " ++ n)
    Just val -> return val
eval7 (Add x y) = do
  tick
  x' <- eval7 x
  y' <- eval7 y
  case (x', y') of
    (IntVal i, IntVal j) -> return $ IntVal (i + j)
    _ -> throwError "type error in addition"
eval7 (Lambda n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval7 (Apply e1 e2) = do
  tick
  x <- eval7 e1
  y <- eval7 e2
  case x of
    FunVal env' n body ->
      local (const (Map.insert n y env')) (eval7 body)
    _ -> throwError "type error in application"
```
