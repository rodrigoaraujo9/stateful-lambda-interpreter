module Combinator where

import Ast(Term(..), fv)

type Ident = String

-- ver video do slide

-- haskell workshop

-- to do graph reduction use term and stack
--
-- literate haskell for report

data Op = Add | Sub | Mul
  deriving (Eq, Show)

data Comb = Var Ident     -- variables -- var should not be a string for real graph reduction -> should be address (int)
         | Comb :@ Comb   -- application
         | S | K | I      -- basic combinators
         -- extra stuff (extensions)
         | B | C          -- extra combinators
         | Y              -- fixpoint combinator
         | Const Int      -- numbers
         | Prim Op       -- primitive operations
         | IfZero     -- conditional
         deriving (Eq, Show)

infixl 9 :@

compile :: Term -> Comb
compile (Ast.Const c) = Combinator.Const c
compile (Ast.Var x) = Combinator.Var x
compile (App e1 e2) = compile e1 :@ compile e2
compile (e1 Ast.:+ e2) = (Prim Add :@ compile e1) :@ compile e2
compile (e1 Ast.:- e2) = (Prim Sub :@ compile e1) :@ compile e2
compile (e1 Ast.:* e2) = (Prim Mul :@ compile e1) :@ compile e2
compile (Lambda x e) = lambdaT x (compile e)
compile (Let x e1 e2) = compile (App (Lambda x e2) e1)
compile (Fix e) = Y :@ compile e
compile (Ast.IfZero e1 e2 e3) = (Combinator.IfZero :@ compile e1) :@ compile e2 :@ compile e3


lambdaT :: Ident -> Comb -> Comb
lambdaT x (Combinator.Var y)
    | x == y = I
    | otherwise = K :@ Combinator.Var y

lambdaT x c
    | not (isfreeComb x c) = K :@ c

lambdaT x (p :@ Combinator.Var y)
    | x == y && not (isfreeComb x p) = p

lambdaT x (p :@ q)
    | not (isfreeComb x p) = (B :@ p) :@ lambdaT x q
    | not (isfreeComb x q) = (C :@ lambdaT x p) :@ q
    | otherwise = (S :@ lambdaT x p) :@ lambdaT x q

lambdaT _ _ = error "unsupported lambda construction"

-- step of rewrite until arrive at weak normal form
-- for now not graph reduction cause it doesn't really do sharing -> see var
rewrite :: [Comb] -> [Comb]
rewrite (I:x:xs) = x:xs
rewrite (K:p:_:xs) = p:xs
rewrite (S:p:q:r:xs) = ((p :@ r) :@ (q :@ r)) : xs
rewrite (C:p:q:r:xs) = ((p :@ r) :@ q) : xs
rewrite (B:p:q:r:xs) = (p :@ (q :@ r)) : xs
rewrite (Prim Add : Combinator.Const a : Combinator.Const b : xs) = Combinator.Const (a + b) : xs
rewrite (Prim Sub : Combinator.Const a : Combinator.Const b : xs) = Combinator.Const (a - b) : xs
rewrite (Prim Mul : Combinator.Const a : Combinator.Const b : xs) = Combinator.Const (a * b) : xs
rewrite (Combinator.IfZero : Combinator.Const 0 : t : _ :xs) = t:xs
rewrite (Combinator.IfZero : Combinator.Const _ : _ : f :xs) = f:xs
rewrite xs = xs

isfree :: Ident -> Term -> Bool
isfree v t = v `elem` fv t

-- whenever we have application call recursively on e1 unwind
-- find the leftmost, outermost redex by descending left on the spine of @-nodes
unwind :: [Comb] -> [Comb]
unwind ((e1 :@ e2): xs) = unwind (e1:e2:xs)
unwind e = e

evaluate :: [Comb] -> [Comb]
evaluate t
    | isnormalform t1 = t1
    | otherwise = evaluate (rewrite t1)
    where t1 = unwind t

isnormalform :: [Comb] -> Bool -- weak normal form (accepts Lambda)
isnormalform e = rewrite e == e

-- trace keep all intermediary steps in list

-- type State = (Comb, Stack)
-- type Stack = [Comb] --is spine

-- rewrite :: State -> Maybe State
-- rewrite (I, p:xs) = Just (p, xs)
-- rewrite (k, p:q:xs) = Just (p, xs)

isfreeComb :: Ident -> Comb -> Bool
isfreeComb v t = v `elem` fvComb t

fvComb :: Comb -> [Ident]
fvComb (Combinator.Var x) = [x]
fvComb (p :@ q) = fvComb p ++ fvComb q
fvComb S = []
fvComb K = []
fvComb I = []
fvComb B = []
fvComb C = []
fvComb Y = []
fvComb (Combinator.Const _) = []
fvComb (Prim _) = []
fvComb Combinator.IfZero = []
