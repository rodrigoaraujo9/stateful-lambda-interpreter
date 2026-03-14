{-# LANGUAGE BangPatterns #-}

module EvalEnv where
import Ast

eval :: Term -> Env -> Value
eval (Const n) _ = Int n
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
    case eval e1 e of
        Int 0 -> eval e2 e
        Int _ -> eval e3 e
        _ -> error "first argument didn't evaluate to Int in IfZero"
eval (Let x e1 e2) e =
    let v = eval e1 e
    in eval e2 ((x, v) : e)
eval (Fix t) env =
    case eval t env of
        Closure (Lambda x e) env' ->
            let v = Closure e ((x, v) : env')
            in v
        _ -> error "cannot apply non-closure value to fixed point"
