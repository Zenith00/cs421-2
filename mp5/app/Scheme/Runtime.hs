{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

-- TODO
liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp f = PrimFunc p where
  p [x,y] = Number . foldl1 f <$> mapM lowerInt [x,y]
  p xs = throwError $ UnexpectedArgs xs

-- TODO
liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
  p [x] = Number . f <$> (lowerInt x)
  p v = throwError $ UnexpectedArgs v

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean False] = return $ Boolean $ f False
  p [_] = return $ Boolean $ f True
  p v = throwError $ UnexpectedArgs v

-- TODO
myFoldl1 :: (Int -> Int -> Bool) -> [Int] -> Bool
myFoldl1 f [] = True
myFoldl1 f [x] = True
myFoldl1 f (x:y:xs) = (f x y) && (myFoldl1 f (y:xs))

liftCompOp :: (Int -> Int -> Bool) -> Val
liftCompOp f = PrimFunc p where
  p [] = return $ Boolean True
  p xs = Boolean . myFoldl1 f <$> mapM lowerInt xs

--- ### Primtive operations

-- Primitive function `car`
-- TODO
car :: [Val] -> EvalState Val
car [] = throwError $ UnexpectedArgs []
car [List (x:xs)] = return x
car [DottedList (x:xs) y] = return x
car [DottedList [] y] = return y
car x = throwError $ UnexpectedArgs x


-- Primitive function `cdr`
-- TODO
cdr :: [Val] -> EvalState Val
cdr [] = throwError $ UnexpectedArgs []
cdr [List (x:xs)] = return (List xs)
cdr [DottedList (x:xs) y] = return (DottedList xs y)
cdr [DottedList [] y] = return y
cdr x = throwError $ UnexpectedArgs x

-- Primitive function `cons`
-- TODO
cons :: [Val] -> EvalState Val
cons [x, y] = return (flattenList (DottedList [x] y))
cons xs = throwError $ UnexpectedArgs xs

list :: [Val] -> EvalState Val
list lst = return (List lst)            


-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
  append' (List []) x = return x
  append' (List xs) (List ys) = return $ List (xs ++ ys)
  append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
  append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
-- TODO
-- Examples:
--   (apply + '(1 2 3))  => 6
--   (apply car '((1 2 3)))  => 1
applyPrim :: [Val] -> EvalState Val
applyPrim [f, List prams]= apply f prams
applyPrim ap =  throwError $ UnexpectedArgs ap

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6
evalPrim :: [Val] -> EvalState Val
evalPrim [e] = eval e
evalPrim es =  throwError $ UnexpectedArgs es


-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
-- TODO
-- Examples:
--   (= 1 1) => #t
--   (= #f #t) => #f
--   (= #f #f) => #t
--   (= 'a 10) => Type error
--   (= 'a 'b) => Type error
equalSign :: [Val] -> EvalState Val
equalSign [] = return (Boolean True)
equalSign [x] = return (Boolean True)
equalSign (x:y:xs) = 
      case (x, y) of 
         (Number a, Number b) | a == b -> 
                                     (\c -> Boolean (True && (lowerBool c))) <$> equalSign (y:xs)
                              | otherwise -> (\c -> Boolean (False && (lowerBool c))) <$> equalSign (y:xs)
         (Boolean a, Boolean b) | a == b -> 
                                     (\c -> Boolean (True && (lowerBool c))) <$> equalSign (y:xs)
                                | otherwise -> (\c -> Boolean (False && (lowerBool c))) <$> equalSign (y:xs)
         (_, _) -> throwError $ TypeError x

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
-- TODO
-- Examples:
--   (eq? 1 1) => #t
--   (eq? #f #t) => #f
--   (eq? #f #f) => #t
--   (eq? 'a 10) => #f
--   (eq? 'a 'a) => #t
eq :: [Val] -> EvalState Val
eq [] = return (Boolean True)
eq [x] = return (Boolean True)
eq (x:y:xs) = 
      case (x, y) of 
         (Number a, Number b) | a == b -> 
                                     (\c -> Boolean (True && (lowerBool c))) <$> eq (y:xs)
                              | otherwise -> (\c -> Boolean (False && (lowerBool c))) <$> eq (y:xs)
         (Boolean a, Boolean b) | a == b -> 
                                     (\c -> Boolean (True && (lowerBool c))) <$> eq (y:xs)
                                | otherwise -> (\c -> Boolean (False && (lowerBool c))) <$> eq (y:xs)
         (Symbol a, Symbol b) | a == b -> 
                                     (\c -> Boolean (True && (lowerBool c))) <$> eq (y:xs)
                              | otherwise -> (\c -> Boolean (False && (lowerBool c))) <$> eq (y:xs)
         (_, _) -> (\c -> Boolean (False && (lowerBool c))) <$> eq (y:xs)

-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
-- TODO
isList :: [Val] -> EvalState Val
isList [List _] = return (Boolean True) 
isList [_] = return (Boolean False)
isList x = throwError $ UnexpectedArgs x

-- Primitive function `symbol?` predicate
-- TODO
isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol _] = return (Boolean True) 
isSymbol [_] = return (Boolean False)
isSymbol x = throwError $ UnexpectedArgs x

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
-- TODO
isPair :: [Val] -> EvalState Val
isPair [List []] = return (Boolean False) 
isPair [List [x]] = return (Boolean False) 
isPair [List x] = return (Boolean True) 
isPair [DottedList [] x] = return (Boolean False) 
isPair [DottedList _ x] = return (Boolean True) 
isPair [_] = return (Boolean False)
isPair x = throwError $ UnexpectedArgs x

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber [Number _] = return (Boolean True) 
isNumber [_] = return (Boolean False)
isNumber x = throwError $ UnexpectedArgs x

-- Primitive function `boolean?` predicate
-- TODO
isBoolean :: [Val] -> EvalState Val
isBoolean [Boolean _] = return (Boolean True) 
isBoolean [_] = return (Boolean False)
isBoolean x = throwError $ UnexpectedArgs x

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull :: [Val] -> EvalState Val
isNull [List []] = return (Boolean True)
isNull [x] = return (Boolean False)
isNull x = throwError $ UnexpectedArgs x

--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("*", liftIntVargOp (*) 1)
                     , ("/", liftIntVargOp (div) 1)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("not", liftBoolUnaryOp not)
                     , (">", liftCompOp (>))
                     , ("<", liftCompOp (<))
                     , (">=", liftCompOp (>=))
                     , ("<=", liftCompOp (<=))
                     , ("modulo", liftIntBinOp (mod))
                     , ("abs", liftIntUnaryOp (abs))
                     , ("=", PrimFunc equalSign)
                     , ("eq?", PrimFunc eq)
                     , ("car", PrimFunc car)
                     , ("cdr", PrimFunc cdr)
                     , ("cons", PrimFunc cons)
                     , ("list", PrimFunc list)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     , ("number?", PrimFunc isNumber)
                     , ("boolean?", PrimFunc isBoolean)
                     , ("null?", PrimFunc isNull)
                     , ("pair?", PrimFunc isPair)
                     , ("apply", PrimFunc applyPrim)
                     , ("eval", PrimFunc evalPrim)
                     -- TODO: Insert more runtime bindings here
                     ]
