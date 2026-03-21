module CompilerSKI where

import Ast(Term(..), fv)

type Ident = String

-- ver video do slide

-- haskell workshop

-- to do graph reduction use term and stack
--
-- literate haskell for report

-- data Comb = Var Ident      -- variables -- var should not be a string for real graph reduction -> should be address (int)
--          | App Comb Comb   -- application
--          | S | K | I      -- basic combinators
--          -- extra stuff (extensions)
--          | B | C          -- extra combinators
--          | Y              -- fixpoint combinator
--          | Const Int      -- numbers
--          | Prim Op       -- primitive operations
--          | IfZero     -- conditional
--          deriving Show

data Comb = V Ident
          | Comb :@ Comb
          | S | K | I
          | B | C
    deriving (Eq, Show)

infixl 9 :@

compile :: Term -> Comb
compile (Var x) = V x
compile (App e1 e2) = compile e1 :@ compile e2
compile (Lambda x (Var y))
    | x == y = I
compile (Lambda x p)
    | not (isfree x p) = K :@ compile p
compile (Lambda x (App p (Var y)))
    | x == y && not (isfree x p) = compile p
compile (Lambda x (App p q))
    | not (isfree x p) = (B :@ compile p) :@ compile (Lambda x q)
    | not (isfree x q) = (C :@ compile (Lambda x p)) :@ compile q
    | isfree x p && isfree x q = (S :@ compile (Lambda x p)) :@ compile (Lambda x q)
compile _ = error "unsupported term"

-- step of rewrite until arrive at weak normal form
-- for now not graph reduction cause it doesn't really do sharing -> see var
rewrite :: [Comb] -> [Comb]
rewrite (I:x:xs) = x:xs
rewrite (K:p:_:xs) = p:xs
rewrite (S:p:q:r:xs) = ((p :@ r) :@ (q :@ r)) : xs
rewrite (C:p:q:r:xs) = ((p :@ r) :@ q) : xs
rewrite (B:p:q:r:xs) = (p :@ (q :@ r)) : xs
rewrite xs = xs

isfree :: Ident -> Term -> Bool
isfree v t = v `elem` fv t

-- whenever we have application call recursively on e1 unwind
-- find the leftmost, outermost redex by descending left on the spine of @-nodes
unwind :: [Comb] -> [Comb]
unwind ((e1 :@ e2): xs) = unwind (e1:e2:xs)
unwind e = e

evaluate :: [Comb] -> [Comb]
evaluate t
    | isnormalform t1 = t1
    | otherwise = evaluate (rewrite t1)
    where t1 = unwind t

isnormalform :: [Comb] -> Bool -- weak normal form (accepts Lambda)
isnormalform e = rewrite e == e

-- trace keep all intermediary steps in list

-- type State = (Comb, Stack)
-- type Stack = [Comb] --is spine

-- rewrite :: State -> Maybe State
-- rewrite (I, p:xs) = Just (p, xs)
-- rewrite (k, p:q:xs) = Just (p, xs)
