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
            let term   = parse (lexer input)
            let combinator   = compile term
            let result = evaluate [combinator]
            putStrLn ""
            putStrLn ""
            putStrLn "*fun!*"
            putStrLn ""
            putStrLn "*term*"
            print term
            putStrLn ""
            putStrLn "*compiled*"
            print combinator
            putStrLn ""
            putStrLn "*evaluated*"
            print result
