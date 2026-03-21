module Compiler where
import Combinatory
import Lambda

compile :: Term -> Comb
compile (Lambda.Const c) = Combinatory.Const c
compile (Lambda.Var x) = Combinatory.Var x
compile (App e1 e2) = compile e1 :@ compile e2
compile (e1 Lambda.:+ e2) = (Prim Add :@ compile e1) :@ compile e2
compile (e1 Lambda.:- e2) = (Prim Sub :@ compile e1) :@ compile e2
compile (e1 Lambda.:* e2) = (Prim Mul :@ compile e1) :@ compile e2
compile (e1 Lambda.:/ e2) = (Prim Div :@ compile e1) :@ compile e2
compile (Lambda x e) = lambdaT x (compile e)
compile (Let x e1 e2) = compile (App (Lambda x e2) e1)
compile (Fix e) = Y :@ compile e
compile (Lambda.IfZero e1 e2 e3) = (Combinatory.IfZero :@ compile e1) :@ compile e2 :@ compile e3

lambdaT :: Combinatory.Ident -> Comb -> Comb
lambdaT x (Combinatory.Var y)
    | x == y = I
    | otherwise = K :@ Combinatory.Var y
lambdaT x c
    | not (isfreeComb x c) = K :@ c
lambdaT x (p :@ Combinatory.Var y)
    | x == y && not (isfreeComb x p) = p
lambdaT x (p :@ q)
    | not (isfreeComb x p) = (B :@ p) :@ lambdaT x q
    | not (isfreeComb x q) = (C :@ lambdaT x p) :@ q
    | otherwise = (S :@ lambdaT x p) :@ lambdaT x q
lambdaT _ _ = error "*compile time error* unsupported lambda construction"

isfreeComb :: Combinatory.Ident -> Comb -> Bool
isfreeComb v t = v `elem` fvComb t
