{
module Parser where
import Lambda
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
    '/'     { TDivide }
    '('     { TLParen }
    ')'     { TRParen }
    '.'     { TDot }
    '\\'    { TBackslash }

%right in
%left '+' '-'
%left '*' '/'

%%

Term :: { Term }
Term
    : let var '=' Term in Term            { Let $2 $4 $6 }
    | lambda var '.' Term                 { Lambda $2 $4 }
    | '\\' var '.' Term                   { Lambda $2 $4 }
    | fix Term                            { Fix $2 }
    | if iszero Term then Term else Term  { IfZero $3 $5 $7 }
    | Arithmetic                          { $1 }

Arithmetic :: { Term }
Arithmetic
    : Arithmetic '+' Arithmetic           { $1 :+ $3 }
    | Arithmetic '-' Arithmetic           { $1 :- $3 }
    | Arithmetic '*' Arithmetic           { $1 :* $3 }
    | Arithmetic '/' Arithmetic           { $1 :/ $3 }
    | Application                         { $1 }

Application :: { Term }
Application
    : Application Atomic                  { App $1 $2 }
    | Atomic                              { $1 }

Atomic :: { Term }
Atomic
    : int                                 { Const $1 }
    | var                                 { Var $1 }
    | '(' Term ')'                        { $2 }

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
    | TDivide
    | TLParen
    | TRParen
    | TDot
    | TBackslash
 deriving Show
}
