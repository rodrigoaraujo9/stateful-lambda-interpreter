module Main where

import System.Environment (getArgs)

import Parser
import Lexer

import qualified EvaluatorSubstitution as Subst
import qualified EvaluatorEnvironment as Env
import qualified EvaluatorCPS as CPS

main :: IO ()
main = do
    args <- getArgs
    input <- getContents

    let term = parse (lexer input)

    case args of
        ["subst"] -> do
            print term
            print (Subst.eval term)

        ["env"] -> do
            print term
            print (Env.eval term [])

        ["cps"] -> do
            print term
            print (CPS.eval term [] id)
        _ ->
            putStrLn "usage: cabal run -- subst | env | cps"
