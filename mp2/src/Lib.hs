module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Helper Functions
--- -----------------

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal a) (BoolVal b) = BoolVal (f a b)
liftBoolOp _ _          _          = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp f (IntVal a) (IntVal b) = BoolVal (f a b)
liftCompOp _ _          _          = ExnVal "Cannot lift"


--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
  case H.lookup s env of
    Just v -> v
    Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env
   | op == "/" && eval e2 env == IntVal 0 = ExnVal "Division by 0"
   | otherwise = let v1 = eval e1 env
                     v2 = eval e2 env
                     Just f = H.lookup op intOps
                 in liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = 
   let v1 = eval e1 env
       v2 = eval e2 env
       Just f = H.lookup op boolOps
   in liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env =
   let v1 = eval e1 env
       v2 = eval e2 env
       Just f = H.lookup op compOps
   in liftCompOp f v1 v2


--- ### If Expressions

eval (IfExp e1 e2 e3) env
    | eval e1 env == BoolVal True = eval e2 env
    | eval e1 env == BoolVal False = eval e3 env
    | otherwise = ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
 case eval e1 env of 
 CloVal params body env'' -> let env' = axu (zipWith (\x y -> (x, y)) params args) env
                                  where axu :: [(String, Exp)] -> Env -> Env
                                        axu [] e = e
                                        axu ((a, b):abs) e' = axu abs (H.insert a (eval b e') e')
                             in eval body env'
 otherwise -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = 
    let env' = axu pairs env
         where axu :: [(String, Exp)] -> Env -> Env
               axu [] e = e
               axu ((a, b):abs) e' = axu abs (H.insert a (eval b e') e')
    in eval body env'

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env


--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, (H.insert var (eval e env) env))

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt s) penv env = axu ("", penv, env) s 
   where axu :: (String, PEnv, Env) -> [Stmt] -> (String, PEnv, Env)
         axu st [] = st
         axu (x, y, z) (d:ds) = 
           let (a, b, c) = exec d y z
           in axu (x++a, b, c) ds

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env 
    | eval e1 env == BoolVal True = exec s1 penv env
    | eval e1 env == BoolVal False = exec s2 penv env
    | otherwise = ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", (H.insert name p penv), env)


exec (CallStmt name args) penv env =
 case H.lookup name penv of 
 Just (ProcedureStmt name params body) -> 
       let env' = axu (zipWith (\x y -> (x, y)) params args) env env
            where axu :: [(String, Exp)] -> Env -> Env -> Env
                  axu [] e en = e
                  axu ((a,b):abs) e' en' = axu abs (H.insert a (eval b en') e') en'
       in exec body penv env'
