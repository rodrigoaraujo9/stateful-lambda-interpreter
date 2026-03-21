module Compiler where

import Combinatory
import Lambda

compile' :: Term -> Comb
compile' t = fst (compile [] 0 t)

compile :: Env -> Addr -> Term -> (Comb, Addr)
compile env next term =
   case term of
     Lambda.Const c ->
       (Combinatory.Const c, next)
     Lambda.Var x ->
       case lookup x env of
         Just x'  -> (Combinatory.Var x', next)
         Nothing  -> error $ "*compile time error* free variable " ++ x
     App e1 e2 ->
       let (c1, a1) = compile env next e1
           (c2, a2) = compile env a1   e2
       in (c1 :@ c2, a2)
     e1 Lambda.:+ e2 ->
       let (c1, a1) = compile env next e1
           (c2, a2) = compile env a1   e2
       in ((Prim Add :@ c1) :@ c2, a2)
     e1 Lambda.:- e2 ->
       let (c1, a1) = compile env next e1
           (c2, a2) = compile env a1   e2
       in ((Prim Sub :@ c1) :@ c2, a2)
     e1 Lambda.:* e2 ->
       let (c1, a1) = compile env next e1
           (c2, a2) = compile env a1   e2
       in ((Prim Mul :@ c1) :@ c2, a2)
     e1 Lambda.:/ e2 ->
       let (c1, a1) = compile env next e1
           (c2, a2) = compile env a1   e2
       in ((Prim Div :@ c1) :@ c2, a2)
     Lambda x e ->
       let x'        = next
           env'      = (x, x') : env
           (c, a) = compile env' (next + 1) e
       in (lambdaT x' c, a)
     Let x e1 e2 ->
       compile env next (App (Lambda x e2) e1)
     Fix e ->
       let (c, a) = compile env next e
       in (Y :@ c, a)
     Lambda.IfZero e1 e2 e3 ->
       let (c1, a1) = compile env next e1
           (c2, a2) = compile env a1 e2
           (c3, a3) = compile env a2 e3
        in (((Combinatory.IfZero :@ c1) :@ c2) :@ c3, a3)

lambdaT :: Addr -> Comb -> Comb
lambdaT x (Combinatory.Var y)
   | x == y    = I
   | otherwise = K :@ Combinatory.Var y
lambdaT x c
   | not (isfree x c) = K :@ c
lambdaT x (p :@ Combinatory.Var y)
   | x == y && not (isfree x p) = p
lambdaT x (p :@ q)
   | not (isfree x p) = (B :@ p) :@ lambdaT x q
   | not (isfree x q) = (C :@ lambdaT x p) :@ q
   | otherwise        = (S :@ lambdaT x p) :@ lambdaT x q
lambdaT _ _ =
   error "*compile time error* unsupported lambda construction"

isfree :: Addr -> Comb -> Bool
isfree v t = v `elem` Combinatory.fv t
