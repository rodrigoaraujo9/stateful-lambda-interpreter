{-# LANGUAGE BangPatterns #-}

module EvaluatorSubstitution where
import Ast

eval :: Term -> Term
eval (Const n)    = Const n
eval (Var _)      = error "free variable occurrence"
eval (Lambda x e) = Lambda x e
eval (App e1 e2) =
  let !e1' = eval e1
      !e2' = eval e2
  in case e1' of
    Lambda x e -> eval (subst e2' x e)
    _ -> error "application exception"
eval (e1 :+ e2) =
  case (eval e1, eval e2) of
    (Const e1', Const e2') -> Const (e1' + e2')
    _ -> error "type error in :+"
eval (e1 :- e2) =
  case (eval e1, eval e2) of
    (Const e1', Const e2') -> Const (e1' - e2')
    _ -> error "type error in :-"
eval (e1 :* e2) =
  case (eval e1, eval e2) of
    (Const e1', Const e2') -> Const (e1' * e2')
    _ -> error "type error in :*"
eval (IfZero e1 e2 e3) =
  case eval e1 of
    Const 0 -> eval e2
    Const _ -> eval e3
    _       -> error "type error in ifzero"
eval (Let x e1 e2) = eval (App (Lambda x e2) e1)
eval (Fix t) =
  case eval t of
    Lambda x e -> eval (subst (Fix t) x e)
    _ -> error "fix exception"

subst :: Term -> Ident -> Term -> Term
subst _ _ (Const c) = Const c
subst v x (Var y)
    | x == y = v
    | otherwise = Var y
subst v x (Lambda y e)
  | x == y = Lambda y e
  | otherwise = Lambda y (subst v x e)
subst v x (App e1 e2) = App (subst v x e1) (subst v x e2)
subst v x (e1 :+ e2) = subst v x e1 :+ subst v x e2
subst v x (e1 :- e2) = subst v x e1 :- subst v x e2
subst v x (e1 :* e2) = subst v x e1 :* subst v x e2
subst v x (IfZero e1 e2 e3) = IfZero (subst v x e1) (subst v x e2) (subst v x e3)
subst v x (Let y e1 e2)
  | x == y = Let y (subst v x e1) e2
  | otherwise = Let y (subst v x e1) (subst v x e2)
subst v x (Fix e) = Fix (subst v x e)
