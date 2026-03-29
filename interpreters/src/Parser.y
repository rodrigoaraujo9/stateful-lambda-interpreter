{
module Parser where
import Ast
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    let     { TLet }
    in      { TIn }
    if      { TIf }
    iszero  { TIsZero }
    then    { TThen }
    else    { TElse }
    fix     { TFix }
    lambda  { TLambda }
    int     { TInt $$ }
    var     { TVar $$ }
    '='     { TEq }
    '+'     { TPlus }
    '-'     { TMinus }
    '*'     { TTimes }
    '('     { TLParen }
    ')'     { TRParen }
    '.'     { TDot }
    '\\'    { TBackslash }

%right in
%left '+' '-'
%left '*'

%%

Term :: { Term }
Term
    : let var '=' Term in Term           { Let $2 $4 $6 }
    | lambda var '.' Term                { Lambda $2 $4 }
    | '\\' var '.' Term                  { Lambda $2 $4 }
    | fix Term                           { Fix $2 }
    | if iszero Term then Term else Term { IfZero $3 $5 $7 }
    | ArithmeticTerm                     { $1 }

ArithmeticTerm :: { Term }
ArithmeticTerm
    : ArithmeticTerm '+' ArithmeticTerm  { $1 :+ $3 }
    | ArithmeticTerm '-' ArithmeticTerm  { $1 :- $3 }
    | ArithmeticTerm '*' ArithmeticTerm  { $1 :* $3 }
    | ApplicationTerm                    { $1 }

ApplicationTerm :: { Term }
ApplicationTerm
    : ApplicationTerm AtomicTerm         { App $1 $2 }
    | AtomicTerm                         { $1 }

AtomicTerm :: { Term }
AtomicTerm
    : int                                { Const $1 }
    | var                                { Var $1 }
    | '(' Term ')'                       { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TLet
    | TIn
    | TIf
    | TIsZero
    | TThen
    | TElse
    | TFix
    | TLambda
    | TInt Int
    | TVar String
    | TEq
    | TPlus
    | TMinus
    | TTimes
    | TLParen
    | TRParen
    | TDot
    | TBackslash
 deriving Show
}
