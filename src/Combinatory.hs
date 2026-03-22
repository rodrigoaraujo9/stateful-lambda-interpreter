module Combinatory where

import Data.List (union)

type Ident = String
type State = (Comb, Stack)
type Stack = [Comb] -- is spine

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

fv :: Comb -> [Ident]
fv (Var x)  = [x]
fv (p :@ q) = fv p `union` fv q
fv _        = []
