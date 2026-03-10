{-# LANGUAGE BangPatterns #-}

module Eval where
import Ast

eval :: Term -> Env -> Value
eval (Const n) _ = (Int n)
eval (Var x) e =
  case lookup x e of
    Just v  -> v
    Nothing -> error "free variable occurrence"
eval (Lambda x ex) e = Closure (Lambda x ex) e
eval (App e1 e2) env =
    let !v1 = eval e1 env
        !v2 = eval e2 env
    in case v1 of
        Closure (Lambda x e) env' -> eval e ((x, v2) : env')
        _ -> error "first argument didn't evaluate to Closure (Lambda x expression) environment"
eval (e1 :+ e2) e =
  case (eval e1 e, eval e2 e) of
    (Int n1, Int n2) -> Int (n1 + n2)
    _ -> error "some argument or both didn't evaluate to Int :+"
eval (e1 :- e2) e =
    case (eval e1 e, eval e2 e) of
    (Int n1, Int n2) -> Int (n1 - n2)
    _ -> error "some argument or both didn't evaluate to Int :-"
eval (e1 :* e2) e =
    case (eval e1 e, eval e2 e) of
    (Int n1, Int n2) -> Int (n1 * n2)
    _ -> error "some argument or both didn't evaluate to Int :*"
eval (IfZero e1 e2 e3) e =
    case (eval e1 e) of
    Int 0 -> eval e2 e
    Int _ -> eval e3 e
    _ -> error "first argument didn't evaluate to Int in IfZero"
eval (Let x e1 e2) e =
    let v = eval e1 e in
    eval e2 ((x,v):e)
eval (Fix t) env =
    case eval t env of
    Closure (Lambda x e) env' ->
        let v = Closure e ((x,v):env') in v
    _ -> error "cannot apply non-closure value to fixed point"

eval_cps :: Term -> Env -> Cont -> Value
eval_cps (Const n) _ c = c (Int n)
eval_cps (Var x) e c =
    case lookup x e of
    Just v  -> c v
    Nothing -> error "free variable occurrence"
eval_cps (Lambda x ex) e c = c (Closure (Lambda x ex) e)
eval_cps (App e1 e2) env c =
  eval_cps e1 env (\v1 ->
    eval_cps e2 env (\v2 ->
      case v1 of
        Closure (Lambda x e) env' -> eval_cps e ((x, v2) : env') c
        _ -> error "type error in application"
    ))
eval_cps (e1 :+ e2) env c =
    eval_cps e1 env (\v1 ->
        eval_cps e2 env
            (\v2 ->
                case (v1,v2) of
                (Int a, Int b) -> c (Int (a+b))
                _ -> error "type error in :+"
            ))
eval_cps (e1 :- e2) env c =
    eval_cps e1 env (\v1 ->
        eval_cps e2 env
            (\v2 ->
                case (v1,v2) of
                (Int a, Int b) -> c (Int (a-b))
                _ -> error "type error in :-"
            ))
eval_cps (e1 :* e2) env c =
    eval_cps e1 env (\v1 ->
        eval_cps e2 env
            (\v2 ->
                case (v1,v2) of
                (Int a, Int b) -> c (Int (a*b))
                _ -> error "type error in :*"
            ))
eval_cps (IfZero e1 e2 e3) env c =
    eval_cps e1 env (\v ->
        case v of
            Int 0 -> eval_cps e2 env c
            Int _ -> eval_cps e3 env c
            _ -> error "first argument didn't evaluate to Int in IfZero"
    )
eval_cps (Let x e1 e2) e c =
    eval_cps e1 e (\v ->
        eval_cps e2 ((x, v): e) c
    )
eval_cps (Fix t) env c =
    eval_cps t env (\v ->
        case v of
            Closure (Lambda f body) env' ->
                let v' = Closure body ((f, v') : env')
                in c v'
            _ -> error "cannot apply fixed point to non-closure"
        )
