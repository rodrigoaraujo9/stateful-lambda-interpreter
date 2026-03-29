module Lexer where
import Parser (Token(..))
import Data.Char

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexWord (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('=':cs)  = TEq      : lexer cs
lexer ('+':cs)  = TPlus    : lexer cs
lexer ('-':cs)  = TMinus   : lexer cs
lexer ('*':cs)  = TTimes   : lexer cs
lexer ('(':cs)  = TLParen  : lexer cs
lexer (')':cs)  = TRParen  : lexer cs
lexer ('.':cs)  = TDot     : lexer cs
lexer ('\\':cs) = TBackslash : lexer cs
lexer (c:_)     = error $ "Unexpected character: " ++ [c]

lexNum :: String -> [Token]
lexNum cs = TInt (read num) : lexer rest
    where (num, rest) = span isDigit cs

lexWord :: String -> [Token]
lexWord cs = tok : lexer rest
    where
        (word, rest) = span isAlphaNum cs
        tok = case word of
            "let"    -> TLet
            "in"     -> TIn
            "if"     -> TIf
            "iszero" -> TIsZero
            "then"   -> TThen
            "else"   -> TElse
            "fix"    -> TFix
            "lambda" -> TLambda
            _        -> TVar word
