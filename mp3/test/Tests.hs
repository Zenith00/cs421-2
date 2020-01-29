--- Getting Started
--- ===============

--- Testing Your Code
--- -----------------
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.HashMap.Strict as H (HashMap, empty, singleton, toList, fromList,
                                 insert, lookup, union, delete, null)

import Test.QuickCheck

import Data.Foldable
import qualified Data.List as List
import Data.List ((\\))
import GHC.Generics (Generic)

import Lib

main :: IO ()
main = defaultMain tests



tests =
  [ testGroup
      "Evenoddk Function"
      [ testProperty
          "Evenoddk works correctly (5 points)"
          prop_eq_evenoddk_evenodd
      ]
    , testGroup
      "Evenoddk Function use two continuations"
      [ testProperty "Evenoddk use continuation 1 (5 points)" (expectFailure prop_evenoddk_usesContinuations1),
        testProperty "Evenoddk use continuation 2 (5 points)" (expectFailure prop_evenoddk_usesContinuations2)
      ]
    , testGroup
      "factk Function"
      [ testProperty
          "Factk works correctly (5 points)"
          prop_factk_eq_fact
      ]
    , testGroup
      "factk Function use continuation"
      [
        testProperty "Factk use continuation (5 points)" (expectFailure prop_factk_usesContinuation)
      ]
  , testGroup
    "isSimple tests"
    [
      testProperty "isSimple tests (5 points)" tests_isSimple
    ]
    , testGroup
      "cpsExp: Integer and Variable expressions"
      [
        testProperty "cpsExp: Integer and Variable expressions (5 points)" tests_cpsExp_intVar
      ]
  , testGroup
    "Basic ap expressions"
    [
      testProperty "Basic ap expressions with only VarExp and IntExp (5 points)" tests_cpsExp_ap_basic
    ]
  , testGroup
    "cpsExp: Application expressions"
    [
      testProperty "cpsExp: Application expressions (5 points)" tests_cpsExp_ap
    ]
    , testGroup
      "Basic Operator expressions"
      [
        testProperty "Basic Operator expressions with only VarExp and IntExp (5 points)" tests_cpsExp_op_basic
      ]
    , testGroup
      "Simple Operator expressions"
      [
        testProperty "Simple Operator expressions (5 points)" tests_cpsExp_simpOp
      ]
  , testGroup
    "cpsExp: Non-Simple Operator expressions"
    [
      testProperty "cpsExp: Non-Simple Operator expressions (5 points)" tests_cpsExp_nonSimpOp
    ]
    , testGroup
      "Basic If expressions"
      [
        testProperty "Basic If expressions with only VarExp and IntExp (5 points)" tests_cpsExp_if_basic
      ]
    , testGroup
      "cpsExp: Simple If expressions"
      [
        testProperty "cpsExp: Simple If expressions (5 points)" tests_cpsExp_simpIf
      ]
    , testGroup
      "cpsExp: totally arbitrary expressions"
      [
        testProperty "cpsExp: totally arbitrary expressions (5 points)" tests_cpsExp_arbitrary
      ]
    , testGroup
      "cpsDecl tests"
      [
        testProperty "cpsDecl tests (5 points)" tests_cpsDecl
      ]
    ]




--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`
fact :: Integer -> Integer
fact x | x <= 0 = 1
       | otherwise = x * (fact (x - 1))

prop_factk_eq_fact :: Positive (Small Integer) -> Property
prop_factk_eq_fact (Positive (Small n)) = factk n id === fact n .&.
                                          factk n ((*2) . (+1)) === 2 * (fact n + 1) .&.
                                          factk n (show . (`div` 2)) === show ((fact n) `div` 2)

prop_factk_usesContinuation :: Integer -> Property
prop_factk_usesContinuation x = factk x (\v -> error "") === fact x



-- --- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`
evenodd :: [Integer] -> Integer
evenodd lst = aux lst 0 0
    where aux [] _ _ = 0
          aux [x] evensum oddsum | x `mod` 2 == 0 = evensum + x
                                 | otherwise      = oddsum + x
          aux (x:xs) evensum oddsum | x `mod` 2 == 0 = aux xs (evensum + x) oddsum
                                    | otherwise      = aux xs evensum (oddsum + x)

prop_evenoddk_usesContinuations1 :: (NonEmptyList Integer) -> Property
prop_evenoddk_usesContinuations1 (NonEmpty lst) = evenoddk lst (\x -> error "") id === evenodd lst

prop_evenoddk_usesContinuations2 :: (NonEmptyList Integer) -> Property
prop_evenoddk_usesContinuations2 (NonEmpty lst) = evenoddk lst id (\x -> error "") === evenodd lst

prop_eq_evenoddk_evenodd :: (NonEmptyList Integer) -> Property
prop_eq_evenoddk_evenodd (NonEmpty lst) = evenoddk lst id id === evenodd lst .&.
                           evenoddk lst (+1) (+1) === evenodd lst + 1 .&.
                           evenoddk lst (*2) (*2) === (evenodd lst) * 2



--- Automated Translation
--- ---------------------

---------------------
--Arbitrary Intances-
---------------------

--1. Arbitrary Expressions
instance Arbitrary Exp where
    arbitrary = sized randSimpleExp
        where randSimpleExp n | n > 2  = randSimpleExp 2
                              | n == 0 = oneof
                                             [ do e1 <- arbitrary
                                                  return $ IntExp e1
                                             , do e1 <- arbitrary
                                                  c <- elements ['a'..'n']
                                                  return $ VarExp $ c : e1
                                             ]
                              | otherwise = oneof
                                            [ do e1 <- arbitrary
                                                 return $ IntExp e1
                                            , do e1 <- arbitrary
                                                 c <- elements ['a'..'n']
                                                 return (VarExp $ c : e1)
                                            , do e1 <- randSimpleExp (n-1)
                                                 e2 <- randSimpleExp (n-1)
                                                 e3 <- randSimpleExp (n-1)
                                                 return $ IfExp e1 e2 e3
                                            , do e2 <- randSimpleExp (n-1)
                                                 e3 <- randSimpleExp (n-1)
                                                 return $ OpExp "op" e2 e3
                                            , do e1 <- randSimpleExp (n-1)
                                                 e2 <- randSimpleExp (n-1)
                                                 return $ AppExp e1 e2
                                             ]

--2. My Arbitrary Simple Expressions(without AppExp)
data SimpleExpr = SimpleExpr Exp
                | EmptyS Integer  --for ending

instance Show SimpleExpr where
    show (SimpleExpr e) = show e
    show (EmptyS n)     = show n


isAppExp :: Exp -> Bool
isAppExp (AppExp _ _) = True
isAppExp _ = False

getBack :: SimpleExpr -> Exp
getBack (EmptyS _) = IntExp 1
getBack (SimpleExpr k) | isAppExp k = let AppExp e1 e2 = k
                                      in  e1
                       | otherwise  = k


deriving instance Generic SimpleExpr

instance Arbitrary SimpleExpr where
   arbitrary = sized randomSimpleExpr
       where randomSimpleExpr n | n > 2 = randomSimpleExpr 2
                                | n == 0 = do e <- arbitrary
                                              return (EmptyS e)
                                | otherwise = do e <- randomSimpleExpr (n - 1)
                                                 return (SimpleExpr (getBack e))
   shrink = genericShrink


--3. My Arbitrary Non-Simple Expression (force the expression to have AppExp)
data NonSimpleExpr = NonSimpleExpr Exp
                   | EmptyNS Integer    --for ending

instance Show NonSimpleExpr where
   show (NonSimpleExpr e) = show e
   show (EmptyNS n)     = show n

getExp2 :: NonSimpleExpr -> Exp
getExp2 (EmptyNS _) = AppExp (VarExp "k") (IntExp 1)
getExp2 (NonSimpleExpr k) = k

deriving instance Generic NonSimpleExpr

instance Arbitrary NonSimpleExpr where
  arbitrary = sized randomNonSimpleExpr
       where randomNonSimpleExpr n | n > 2  = randomNonSimpleExpr 2
                                   | n == 1 = do e <- randomNonSimpleExpr (n - 1)
                                                 if isAppExp (getExp2 e) then return (NonSimpleExpr (getExp2 e)) else (randomNonSimpleExpr 1) --loop on 1 until it gets a AppExp, and move on
                                   | n == 0 = do e <- arbitrary
                                                 return (EmptyNS e)
                                   | otherwise = do e <- randomNonSimpleExpr (n - 1)
                                                    return (NonSimpleExpr (getExp2 e))
  shrink = genericShrink


-- 4. Operator Strings
newtype Ops = Ops { getString :: String }
    deriving (Show)
instance Arbitrary Ops where
    arbitrary = Ops <$> (elements $  ["+", "-", "*", "/", "<", ">", "<=", ">=", "/=", "=="])


-------------------------
--All the Test Functions-
-------------------------

-- tests_isSimple :: SimpleExpr -> NonSimpleExpr -> Property
-- tests_isSimple (SimpleExpr e1) (NonSimpleExpr e2)  = isSimple e1 === True .&. isSimple e2 === False
-- tests_isSimple (SimpleExpr e) (EmptyNS _)          = isSimple e === True
-- tests_isSimple (EmptyS _)       (NonSimpleExpr e)  = isSimple e === False
-- tests_isSimple _                _                  = True === True

--use totally arbitrary expressions
tests_isSimple :: Exp -> Property
tests_isSimple e = isSimple e === isSimple_test e

--use totally arbitrary expressions
tests_cpsExp_arbitrary :: Exp -> Property
tests_cpsExp_arbitrary e = cpsExp e (VarExp "k") 0 === cpsExp_test e (VarExp "k") 0


tests_cpsExp_intVar :: Integer -> String -> Property
tests_cpsExp_intVar e1 e2 = cpsExp (IntExp e1) (VarExp "k") 0 === cpsExp_test (IntExp e1) (VarExp "k") 0 .&.
                            cpsExp (VarExp e2) (VarExp "k") 0 === cpsExp_test (VarExp e2) (VarExp "k") 0


--OpExp
tests_cpsExp_op_basic :: Ops -> Integer -> Integer -> String -> String -> Property
tests_cpsExp_op_basic (Ops op) x1 x2 s1 s2 =
    cpsExp (OpExp op (IntExp x1) (IntExp x2)) (VarExp "k") 0 === cpsExp_test (OpExp op (IntExp x1) (IntExp x2)) (VarExp "k") 0 .&.
    cpsExp (OpExp op (VarExp s1) (VarExp s2)) (VarExp "k") 0 === cpsExp_test (OpExp op (VarExp s1) (VarExp s2)) (VarExp "k") 0


tests_cpsExp_simpOp ::  Ops -> Ops -> Ops -> Integer -> Integer -> Integer -> Integer -> Property
tests_cpsExp_simpOp (Ops op1) (Ops op2) (Ops op3) x1 x2 x3 x4 =
    let e1 = OpExp op1 (OpExp op2 (IntExp x1) (IntExp x2)) (IntExp x3)
        e2 = OpExp op1 (IntExp x1) (OpExp op2 (IntExp x2) (IntExp x3))
        e3 = OpExp op1 (OpExp op2 (IntExp x1) (IntExp x2)) (OpExp op3 (IntExp x3) (IntExp x4))
    in  cpsExp e1 (VarExp "k") 0 === cpsExp_test e1 (VarExp "k") 0 .&.
        cpsExp e2 (VarExp "k") 0 === cpsExp_test e2 (VarExp "k") 0 .&.
        cpsExp e3 (VarExp "k") 0 === cpsExp_test e3 (VarExp "k") 0


tests_cpsExp_nonSimpOp :: Ops -> Ops -> String -> String -> Integer -> Integer -> Property
tests_cpsExp_nonSimpOp  (Ops op1) (Ops op2) s1 s2 x1 x2 =
    let e1 = OpExp op1 (IntExp x1) (AppExp (VarExp s1) (VarExp s2))
        e2 = OpExp op1 (IntExp x1) (OpExp op2 (IntExp x2) (AppExp (VarExp s1) (VarExp s2)))
    in  cpsExp e1 (VarExp "k") 0 === cpsExp_test e1 (VarExp "k") 0 .&.
        cpsExp e2 (VarExp "k") 0 === cpsExp_test e2 (VarExp "k") 0


--AppExp
tests_cpsExp_ap_basic :: String -> Integer -> Property
tests_cpsExp_ap_basic v x = cpsExp (AppExp (VarExp v) (IntExp x)) (VarExp "k") 0 === cpsExp_test (AppExp (VarExp v) (IntExp x)) (VarExp "k") 0


tests_cpsExp_ap :: Ops -> String -> String -> String -> Integer -> Integer -> Property
tests_cpsExp_ap (Ops op) s1 s2 s3 x1 x2 =
    let e1 = AppExp (VarExp s1) (VarExp s2)
        e2 = AppExp (VarExp s1) (OpExp op (VarExp s2) (IntExp x1))
        e3 = AppExp (VarExp s1) (AppExp (VarExp s2) (AppExp (VarExp s3) (IntExp x1)))
        e4 = AppExp (VarExp s1) (AppExp (VarExp s2) (AppExp (VarExp s3) (OpExp op (IntExp x1) (IntExp x2))))
    in  cpsExp e1 (VarExp "k") 0 === cpsExp_test e1 (VarExp "k") 0 .&.
        cpsExp e2 (VarExp "k") 0 === cpsExp_test e2 (VarExp "k") 0 .&.
        cpsExp e3 (VarExp "k") 0 === cpsExp_test e3 (VarExp "k") 0 .&.
        cpsExp e4 (VarExp "k") 0 === cpsExp_test e4 (VarExp "k") 0


--IfExp
tests_cpsExp_if_basic :: String -> Integer -> Integer -> Property
tests_cpsExp_if_basic s x1 x2  =
    cpsExp (IfExp (VarExp s) (IntExp x1) (IntExp x2)) (VarExp "k") 0 === cpsExp_test (IfExp (VarExp s) (IntExp x1) (IntExp x2)) (VarExp "k") 0



tests_cpsExp_simpIf :: Ops -> Ops -> Ops -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Property
tests_cpsExp_simpIf  (Ops op1) (Ops op2) (Ops op3) x1 x2 x3 x4 x5 x6 =
    let e1 = IfExp (OpExp op1 (IntExp x1) (IntExp x2)) (IntExp x3) (IntExp x4)
        e2 = IfExp (OpExp op1 (IntExp x1) (IntExp x2)) (OpExp op2 (IntExp x3) (IntExp x4)) (IntExp x5)
        e3 = IfExp (OpExp op1 (IntExp x1) (IntExp x2)) (OpExp op2 (IntExp x3) (IntExp x4)) (OpExp op3 (IntExp x5) (IntExp x6))
    in  cpsExp e1 (VarExp "k") 0 === cpsExp_test e1 (VarExp "k") 0 .&.
        cpsExp e2 (VarExp "k") 0 === cpsExp_test e2 (VarExp "k") 0 .&.
        cpsExp e3 (VarExp "k") 0 === cpsExp_test e3 (VarExp "k") 0
--- ### Define `cpsExp` - Overview



tests_cpsExp_nonSimpIf :: Ops -> Ops -> String -> String -> String -> String -> Integer -> Integer -> Integer -> Integer -> Property
tests_cpsExp_nonSimpIf (Ops op1) (Ops op2) s1 s2 s3 s4 x1 x2 x3 x4 =
    let e1 = IfExp (AppExp (VarExp s1) (OpExp s2 (IntExp x1) (VarExp s3))) (OpExp op1 (IntExp x2) (IntExp x3)) (IntExp x4)
        e2 = IfExp (OpExp op1 (IntExp x1) (AppExp (VarExp s1) (VarExp s2))) (OpExp op2 (IntExp x2) (AppExp (VarExp s3) (VarExp s4))) (IntExp x3)
        e3 = OpExp op1 (OpExp op2 (IntExp x1) (IntExp x2)) (IfExp (AppExp (VarExp s1) (VarExp s2)) (VarExp s3) (AppExp (VarExp s4) (IntExp x3)))
    in  cpsExp e1 (VarExp "k") 0 === cpsExp_test e1 (VarExp "k") 0 .&.
        cpsExp e2 (VarExp "k") 0 === cpsExp_test e2 (VarExp "k") 0 .&.
        cpsExp e3 (VarExp "k") 0 === cpsExp_test e3 (VarExp "k") 0




tests_cpsDecl :: String -> Ops -> String -> String -> Integer -> Property
tests_cpsDecl f (Ops op) s1 s2 x1 =
    let d1 = Decl f [s1] (OpExp op (VarExp s1) (IntExp x1))
        d2 = Decl f [s1,s2] (IfExp (OpExp op (VarExp s1) (IntExp x1)) (AppExp (VarExp s2) (VarExp s1)) (AppExp (VarExp s2) (AppExp (VarExp s2) (VarExp s1))))
    in  cpsDecl d1  === cpsDecl_test d1 .&.
        cpsDecl d2  === cpsDecl_test d2



--- Our own slolutions
--- ========
--- ### Define `isSimple`

isSimple_test :: Exp -> Bool
isSimple_test (VarExp _)       = True
isSimple_test (IntExp _)       = True
isSimple_test (AppExp _ _)     = False
isSimple_test (IfExp e1 e2 e3) = all isSimple [e1,e2,e3]
isSimple_test (OpExp op e1 e2) = isSimple e1 && isSimple e2
isSimple_test (LamExp _ _)     = True


--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp_test :: Exp -> Exp -> Integer -> (Exp, Integer)

cpsExp_test e@(IntExp _) k syms = (AppExp k e, syms)
cpsExp_test e@(VarExp _) k syms = (AppExp k e, syms)

--- #### Define `cpsExp_test` for Application Expressions

cpsExp_test (AppExp f arg) k syms
    | isSimple arg = (AppExp (AppExp f arg) k, syms)
    | otherwise    = let (name,s2) = gensym syms
                     in  cpsExp_test arg (LamExp name $ AppExp (AppExp f (VarExp name)) k) s2

--- #### Define `cpsExp_test` for Operator Expressions

cpsExp_test e@(OpExp op e1 e2) k syms
    | isSimple e1 && isSimple e2 = (AppExp k e, syms)
    | isSimple e1 = let (v,s2) = gensym syms
                    in  cpsExp_test e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) s2
    | isSimple e2 = let (v,s2) = gensym syms
                    in  cpsExp_test e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) s2
    | otherwise   = let (v1,s2)  = gensym syms
                        (v2,s3)  = gensym s2
                        base     = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
                        (ce2,s4) = cpsExp_test e2 base s3
                    in  cpsExp_test e1 (LamExp v1 ce2) s4

--- #### Define `cpsExp_test` for If Expressions

cpsExp_test (IfExp c tb fb) k syms
    | isSimple c = let (tb',s2) = cpsExp_test tb k syms
                       (fb',s3) = cpsExp_test fb k s2
                   in  (IfExp c tb' fb', s3)
    | otherwise  = let (v,s2)   = gensym syms
                       (tb',s3) = cpsExp_test tb k s2
                       (fb',s4) = cpsExp_test fb k s3
                   in  cpsExp_test c (LamExp v $ IfExp (VarExp v) tb' fb') s4

--- ### Define `cpsDecl`

cpsDecl_test :: Stmt -> Stmt
cpsDecl_test (Decl f params body)
    = let f'         = f
          params'    = params ++ ["k"]
          (body', _) = cpsExp_test body (VarExp "k") 1
      in  Decl f' params' body'

