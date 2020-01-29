
module Infer where

import Common

import Data.Map.Strict as H (Map, insert, lookup, empty)

  {- question 1: fresh instance function -}

changeFresh [] senv = return senv
changeFresh (x:xs) senv = 
     do tau <- freshTau
        val <- changeFresh xs (substCompose (substInit x tau) senv)
        return val

freshInst :: PolyTy -> FVState MonoTy
freshInst (qVars, tau) = 
     do senv <- changeFresh qVars substEmpty
        return (liftMonoTy senv tau)

freshInstFV :: FVState PolyTy -> FVState MonoTy
freshInstFV s = s >>= (\tau -> freshInst tau)

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = case tau of
        TyVar id | id == i -> True
                 | otherwise -> False
        TyConst s [] -> False
        TyConst s (x:xs) -> occurs i x || occurs i (TyConst s xs)

  {- question 3: unification -}

unify :: Exp -> [(MonoTy, MonoTy)] -> FVState SubstEnv
unify exp eqList = aux exp eqList substEmpty
    where aux :: Exp -> [(MonoTy, MonoTy)] -> SubstEnv -> FVState SubstEnv
          aux exp [] c = return c
          aux exp ((a, b):xs) c 
            | a == b = aux exp xs c
            | otherwise = 
                case (a, b) of 
                   (TyConst s mono, TyVar aa) -> aux exp ((b,a):xs) c
                   (TyConst "pair" [t1, t2], TyConst "pair" [t3, t4]) -> aux exp ((t1,t3):(t2,t4):xs) c
                   (TyConst "fun" [t1, t2], TyConst "fun" [t3, t4]) -> aux exp ((t1,t3):(t2,t4):xs) c
                   (TyConst "list" l1, TyConst "list" l2) -> aux exp ((zip l1 l2) ++ xs) c
                   (TyVar i, t) | occurs i t -> throwError (UnifError exp)
                                | otherwise -> aux exp (map (\(k, h) -> (liftMonoTy (substInit i b) k, liftMonoTy (substInit i b) h)) xs) (substCompose (substInit i b) c)
                   (_, _) -> throwError (UnifError exp)
       

  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> MonoTy -> FVState SubstEnv
infer env exp tau = case exp of
   ConstExp c -> 
      do mono <- freshInstFV (constTySig c)
         val <- unify exp [(tau, mono)]
         return val
   VarExp s -> case H.lookup s env of
      Just t -> do mono <- freshInst t
                   val <- unify exp [(tau, mono)]
                   return val
      Nothing -> throwError (LookupError s)
   LetExp s exp1 exp2 -> 
       do tau1 <- freshTau
          t1 <- infer env exp1 tau1
          t2 <- infer (H.insert s (gen (liftEnv t1 env) (liftMonoTy t1 tau1)) (liftEnv t1 env)) exp2 (liftMonoTy t1 tau)
          return (substCompose t2 t1)
   MonOpExp op exp -> 
       do tau1 <- freshTau
          t1 <- infer env exp tau1
          optp <- monopTySig op
          inst <- freshInst optp
          senv <- unify exp [(liftMonoTy t1 (funTy tau1 tau), inst)]
          return (substCompose senv t1)
   BinOpExp op exp1 exp2 -> 
       do tau1 <- freshTau
          t1 <- infer env exp1 tau1
          tau2 <- freshTau
          t2 <- infer (liftEnv t1 env) exp2 tau2
          inst <- freshInstFV (binopTySig op)
          senv <- unify exp [(liftMonoTy (substCompose t1 t2) (funTy tau1 (funTy tau2 tau)), inst)]
          return (substCompose (substCompose senv t2) t1)
   IfExp exp1 exp2 exp3 -> 
       do t1 <- infer env exp1 boolTy
          t2 <- infer (liftEnv t1 env) exp2 (liftMonoTy t1 tau)
          t3 <- infer (liftEnv (substCompose t1 t2) env) exp3 (liftMonoTy (substCompose t1 t2) tau)
          return (substCompose t3 (substCompose t2 t1))
   FunExp s exp1 ->
       do tau1 <- freshTau
          id1 <- freshTVar
          tau2 <- freshTau
          t1 <- infer (H.insert s ([id1], tau1) env) exp1 tau2
          senv <- unify exp [(liftMonoTy t1 tau, liftMonoTy t1 (funTy tau1 tau2))]
          return (substCompose senv t1)
   AppExp exp1 exp2 -> 
       do tau1 <- freshTau
          t1 <- infer env exp1 (funTy tau1 tau)
          t2 <- infer (liftEnv t1 env) exp2 (liftMonoTy t1 tau1)
          return (substCompose t2 t1)
   LetRecExp f x exp1 exp2 -> 
       do tau1 <- freshTau
          id1 <- freshTVar
          tau2 <- freshTau
          id2 <- freshTVar
          t1 <- infer (H.insert x ([id1], tau1) (H.insert f ([id2], funTy tau1 tau2) env)) exp1 tau2
          t2 <- infer (H.insert f (gen (liftEnv t1 env) (liftMonoTy t1 (funTy tau1 tau2))) (liftEnv t1 env)) exp2 (liftMonoTy t1 tau)
          return (substCompose t1 t2)

inferInit :: TypeEnv -> Exp -> FVState MonoTy
inferInit env e = do
  tau <- freshTau
  sEnv <- infer env e tau
  return (liftMonoTy sEnv tau)

inferDec :: TypeEnv -> Dec -> FVState (TypeEnv, MonoTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x (quantifyMonoTy tau) env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f (quantifyMonoTy tau) env, tau)
