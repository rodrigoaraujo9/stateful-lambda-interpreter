module Evaluator where

import Combinatory

evaluate :: [Comb] -> [Comb]
evaluate t =
  case unwind t of
    (Prim Add : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const x], [Const y]) -> evaluate (Const (x + y) : xs)
        _ -> error "*runtime error* invalid arguments for addition"
    (Prim Sub : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const x], [Const y]) -> evaluate (Const (x - y) : xs)
        _ -> error "*runtime error* invalid arguments for subtraction"
    (Prim Mul : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const x], [Const y]) -> evaluate (Const (x * y) : xs)
        _ -> error "*runtime error* invalid arguments for multiplication"
    (Prim Div : a : b : xs) ->
      case (evaluate [a], evaluate [b]) of
        ([Const _], [Const 0]) -> error "division by zero"
        ([Const x], [Const y]) -> evaluate (Const (x `div` y) : xs)
        _ -> error "*runtime error* invalid arguments for division"
    (IfZero : c : t1 : t2 : xs) ->
      case evaluate [c] of
        [Const 0] -> evaluate (t1 : xs)
        [Const _] -> evaluate (t2 : xs)
        _ -> error "*runtime error* invalid condition in ifzero"
    t'
      | rewrite t' == t' -> t'
      | otherwise        -> evaluate (rewrite t')

unwind :: [Comb] -> [Comb]
unwind ((e1 :@ e2): xs) = unwind (e1:e2:xs)
unwind e = e

rewrite :: [Comb] -> [Comb]
rewrite (I:x:xs)     = x:xs
rewrite (K:p:_:xs)   = p:xs
rewrite (S:p:q:r:xs) = ((p :@ r) :@ (q :@ r)) : xs
rewrite (C:p:q:r:xs) = ((p :@ r) :@ q) : xs
rewrite (B:p:q:r:xs) = (p :@ (q :@ r)) : xs
rewrite (Y:f:xs)     = (f :@ (Y :@ f)) :xs
rewrite xs = xs
