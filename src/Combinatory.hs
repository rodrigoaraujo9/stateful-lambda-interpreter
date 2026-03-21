module Combinatory where

import Data.List (union)

type Addr  = Int
type Stack = [Comb]
type Ident = String
type State = (Comb, Stack)
type Env   = [(Ident, Addr)]

data Op = Add | Sub | Mul | Div
    deriving (Eq, Show)

data Comb = Var Addr      -- variables
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

fv :: Comb -> [Addr]
fv (Var x)   = [x]
fv (p :@ q)  = fv p `union` fv q
fv S         = []
fv K         = []
fv I         = []
fv B         = []
fv C         = []
fv Y         = []
fv (Const _) = []
fv (Prim _)  = []
fv IfZero    = []
