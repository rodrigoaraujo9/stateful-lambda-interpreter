{-# LANGUAGE BangPatterns #-}

module EvalCPS where
import Ast

eval :: Term -> Env -> Cont -> Value
eval (Const n) _ c = c (Int n)
eval (Var x) e c =
    case lookup x e of
        Just v  -> c v
        Nothing -> error "free variable occurrence"
eval (Lambda x ex) e c = c (Closure (Lambda x ex) e)
eval (App e1 e2) env c =
  eval e1 env (\v1 ->
    eval e2 env (\v2 ->
      case v1 of
        Closure (Lambda x e) env' -> eval e ((x, v2) : env') c
        _ -> error "type error in application"
    ))
eval (e1 :+ e2) env c =
    eval e1 env (\v1 ->
        eval e2 env (\v2 ->
            case (v1, v2) of
                (Int a, Int b) -> c (Int (a + b))
                _ -> error "type error in :+"))
eval (e1 :- e2) env c =
    eval e1 env (\v1 ->
        eval e2 env (\v2 ->
            case (v1, v2) of
                (Int a, Int b) -> c (Int (a - b))
                _ -> error "type error in :-"))
eval (e1 :* e2) env c =
    eval e1 env (\v1 ->
        eval e2 env (\v2 ->
            case (v1, v2) of
                (Int a, Int b) -> c (Int (a * b))
                _ -> error "type error in :*"))
eval (IfZero e1 e2 e3) env c =
    eval e1 env (\v ->
        case v of
            Int 0 -> eval e2 env c
            Int _ -> eval e3 env c
            _ -> error "first argument didn't evaluate to Int in IfZero")
eval (Let x e1 e2) e c =
    eval e1 e (\v ->
        eval e2 ((x, v) : e) c)
eval (Fix t) env c =
    eval t env (\v ->
        case v of
            Closure (Lambda f body) env' ->
                let v' = Closure body ((f, v') : env')
                in c v'
            _ -> error "cannot apply fixed point to non-closure")
