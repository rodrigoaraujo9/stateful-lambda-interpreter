module Evaluator where
import Combinatory

evaluate :: [Comb] -> [Comb]
evaluate t
    | isnormalform t' = t'
    | otherwise = evaluate (rewrite t')
    where t' = unwind t

unwind :: [Comb] -> [Comb]
unwind ((e1 :@ e2): xs) = unwind (e1:e2:xs)
unwind e = e

rewrite :: [Comb] -> [Comb]
rewrite (I:x:xs)     = x:xs
rewrite (K:p:_:xs)   = p:xs
rewrite (S:p:q:r:xs) = ((p :@ r) :@ (q :@ r)) : xs
rewrite (C:p:q:r:xs) = ((p :@ r) :@ q) : xs
rewrite (B:p:q:r:xs) = (p :@ (q :@ r)) : xs
rewrite (Prim Add : Combinatory.Const a : Combinatory.Const b : xs) = Combinatory.Const (a + b) : xs
rewrite (Prim Sub : Combinatory.Const a : Combinatory.Const b : xs) = Combinatory.Const (a - b) : xs
rewrite (Prim Mul : Combinatory.Const a : Combinatory.Const b : xs) = Combinatory.Const (a * b) : xs
rewrite (Prim Div : Combinatory.Const _ : Combinatory.Const 0 : _)  = error "division by zero"
rewrite (Prim Div : Combinatory.Const a : Combinatory.Const b : xs) = Combinatory.Const (a `div` b) : xs

rewrite (Combinatory.IfZero : Combinatory.Const 0 : t : _ :xs)      = t:xs
rewrite (Combinatory.IfZero : Combinatory.Const _ : _ : f :xs)      = f:xs
rewrite xs = xs

isnormalform :: [Comb] -> Bool
isnormalform e = rewrite e == e
