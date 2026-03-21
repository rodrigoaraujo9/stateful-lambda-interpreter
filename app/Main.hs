module Main where

import System.Environment (getArgs)

import Parser
import Lexer

import Evaluator
import Compiler


main :: IO ()
main = do
    [file] <- getArgs
    if reverse (take 4 (reverse file)) /= ".fun"
        then error "give a .fun file"
        else do
            input <- readFile file
            let term = parse (lexer input)
            let comb = compile term
            let result = evaluate [comb]
            putStrLn "term:"
            print term
            putStrLn ""
            putStrLn "code:"
            print comb
            putStrLn ""
            putStrLn "result:"
            print result
