module Evaluator where

import Combinatory

eval :: Comb -> Comb
eval c =
  case evaluate (c, []) of
    (result, []) -> result
    (result, xs) -> foldl (:@) result xs

evaluate :: State -> State
evaluate st =
  case unwind st of
    (Prim Add, a:b:xs) ->
      case (eval a, eval b) of
        (Const x, Const y) -> evaluate (Const (x + y), xs)
        _ -> error "*runtime error* invalid arguments for addition"
    (Prim Sub, a:b:xs) ->
      case (eval a, eval b) of
        (Const x, Const y) -> evaluate (Const (x - y), xs)
        _ -> error "*runtime error* invalid arguments for subtraction"
    (Prim Mul, a:b:xs) ->
      case (eval a, eval b) of
        (Const x, Const y) -> evaluate (Const (x * y), xs)
        _ -> error "*runtime error* invalid arguments for multiplication"
    (Prim Div, a:b:xs) ->
      case (eval a, eval b) of
        (_, Const 0)       -> error "*runtime error* division by zero"
        (Const x, Const y) -> evaluate (Const (x `div` y), xs)
        _ -> error "*runtime error* invalid arguments for division"
    (IfZero, c:t1:t2:xs) ->
      case eval c of
        Const 0 -> evaluate (t1, xs)
        Const _ -> evaluate (t2, xs)
        _       -> error "*runtime error* invalid condition in ifzero"
    st' ->
      case rewrite st' of
        Just st'' -> evaluate st''
        Nothing   -> st'

unwind :: State -> State
unwind (e1 :@ e2, xs) = unwind (e1, e2 : xs)
unwind st             = st

rewrite :: State -> Maybe State
rewrite (I, x:xs)     = Just (x, xs)
rewrite (K, p:_:xs)   = Just (p, xs)
rewrite (S, p:q:r:xs) = Just (((p :@ r) :@ (q :@ r)), xs)
rewrite (C, p:q:r:xs) = Just (((p :@ r) :@ q), xs)
rewrite (B, p:q:r:xs) = Just (p :@ (q :@ r), xs)
rewrite (Y, f:xs)     = Just (f :@ (Y :@ f), xs)
rewrite _             = Nothing
