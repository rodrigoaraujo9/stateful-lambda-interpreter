module Lambda where

import Data.List (union, delete)

type Ident = String

data Term = Var Ident               -- variables
          | Lambda Ident Term       -- abstraction
          | App Term Term           -- application
          | Const Int               -- constants
          | Term :+ Term            -- arithmetic operators
          | Term :- Term
          | Term :* Term
          | Term :/ Term
          | IfZero Term Term Term   -- conditional
          | Let Ident Term Term     -- local definition
          | Fix Term                -- fixed-point operator
        deriving (Show, Eq)

fv :: Term -> [Ident]
fv (Var x)           = [x]
fv (Lambda x e)      = delete x (fv e)
fv (App e1 e2)       = fv e1 `union` fv e2
fv (Const _)         = []
fv (e1 :+ e2)        = fv e1 `union` fv e2
fv (e1 :* e2)        = fv e1 `union` fv e2
fv (e1 :- e2)        = fv e1 `union` fv e2
fv (e1 :/ e2)        = fv e1 `union` fv e2
fv (IfZero e1 e2 e3) = fv e1 `union` fv e2 `union` fv e3
fv (Let x e1 e2)     = fv e1 `union` delete x (fv e2)
fv (Fix e)           = fv e
