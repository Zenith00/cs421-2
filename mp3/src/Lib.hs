--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk 1 k = k 1
factk n k = factk (n-1) (\v -> k (n * v)) 

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [] k1 k2 = k1 0
evenoddk [x] k1 k2 | x `rem` 2 == 0 = k1 x
                   | otherwise = k2 x
evenoddk (x:xs) k1 k2 | x `rem` 2 == 0 = evenoddk xs (\v -> k1 (v+x)) k2
                      | otherwise = evenoddk xs k1 (\v -> k2 (v+x))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple exp = 
   case exp of (AppExp exp exp2) -> False
               (IntExp i)  -> True
               (VarExp v)  -> True
               (IfExp exp1 exp2 exp3) -> (isSimple exp1) && (isSimple exp2) && (isSimple exp3)
               (OpExp op exp1 exp2) -> (isSimple exp1) && (isSimple exp2)
               (LamExp var exp) -> True



--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f exp) k n | isSimple exp == True = (AppExp (AppExp f exp) k, n)
                          | otherwise
                               = let (v1, n1) = gensym n
                                 in cpsExp exp (LamExp v1 (AppExp (AppExp f (VarExp v1)) k)) n1
                          
                             
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp string exp1 exp2) k n
   | isSimple exp1 == True && isSimple exp2 == True = ((AppExp k (OpExp string exp1 exp2)), n)
   | isSimple exp1 == True && isSimple exp2 == False
        = let (v, n1) = gensym n
          in cpsExp exp2 (LamExp v (AppExp k (OpExp string exp1 (VarExp v)))) n1
   | isSimple exp1 == False && isSimple exp2 == True 
        = let (v, n1) = gensym n
          in cpsExp exp1 (LamExp v (AppExp k (OpExp string (VarExp v) exp2))) n1
   | otherwise 
        = let (v1, n1) = gensym n
              (v2, n2) = gensym n1
              (e, n3) = cpsExp exp2 (LamExp v2 (AppExp k (OpExp string (VarExp v1) (VarExp v2)))) n2
          in cpsExp exp1 (LamExp v1 e) n3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp exp1 exp2 exp3) k n
   | isSimple exp1 
        = let (e, n1) = cpsExp exp2 k n
              (e1, n2) = cpsExp exp3 k n1
          in (IfExp exp1 e e1, n2)
   | otherwise 
        = let (v, n1) = gensym n
              (e, n2) = cpsExp exp2 k n1
              (e1, n3) = cpsExp exp3 k n2
          in cpsExp exp1 (LamExp v (IfExp (VarExp v) e e1)) n3

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) = let (e, n) = cpsExp body (VarExp "k") 1
                               in Decl f (params++["k"]) e
