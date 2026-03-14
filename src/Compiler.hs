module Compiler where
import Ast(Term(..), Ident)
import Data.List (elemIndex)
import Types

compile :: Term -> [Ident] -> Code
compile (Const n) _ = [LDC n]
compile (Var x) s =
    case elemIndex x s of
        Nothing -> error "free variable occurrence"
        Just k  -> [LD k]
compile (e1 :+ e2) s = compile e1 s ++ compile e2 s ++ [ADD]
compile (e1 :- e2) s = compile e1 s ++ compile e2 s ++ [SUB]
compile (e1 :* e2) s = compile e1 s ++ compile e2 s ++ [MUL]
compile (Lambda x e) s =
    let c = compile e (x:s) ++ [RTN]
    in [LDF c]
compile (App e1 e2) s =
    compile e1 s ++ compile e2 s ++ [AP]
compile (IfZero e1 e2 e3) s =
    let e1'     = compile e1 s
        e2' = compile e2 s ++ [JOIN]
        e3' = compile e3 s ++ [JOIN]
    in e1' ++ [SEL e2' e3']
compile (Let x e1 e2) s =
    compile (App (Lambda x e2) e1) s
compile (Fix (Lambda f (Lambda x e))) s =
    let c = compile e (x:f:s) ++ [RTN]
    in [LDRF c]
compile (Fix _) _ =
    error "Fix expects type Lambda f (Lambda x e)"
