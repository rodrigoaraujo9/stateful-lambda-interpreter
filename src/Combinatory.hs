module Combinatory where

type Ident = String

data Op = Add | Sub | Mul | Div
    deriving (Eq, Show)

data Comb = Var Ident     -- variables
         | Comb :@ Comb   -- application
         | S | K | I      -- basic combinators

         -- extensions
         | B | C          -- extra combinators
         | Y              -- fixpoint combinator
         | Const Int      -- numbers
         | Prim Op        -- primitive operations
         | IfZero         -- conditional
         deriving (Eq, Show)

infixl 9 :@

fvComb :: Comb -> [Ident]
fvComb (Combinatory.Var x) = [x]
fvComb (p :@ q) = fvComb p ++ fvComb q
fvComb S = []
fvComb K = []
fvComb I = []
fvComb B = []
fvComb C = []
fvComb Y = []
fvComb (Combinatory.Const _) = []
fvComb (Prim _) = []
fvComb Combinatory.IfZero = []
