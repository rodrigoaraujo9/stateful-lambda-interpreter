module Main where

import System.Environment (getArgs)

import Parser
import Lexer

import qualified EvalSubst as Subst
import qualified EvalEnv as Env
import qualified EvalCPS as CPS
import qualified CompilerSECD as SECD
import qualified CompilerSKI as SKI
import Run

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

        ["secd"] -> do
            let code = SECD.compile term []
            let result = run code
            print term
            print code
            print result

        ["ski"] -> do
            let comb = SKI.compile term
            let result = SKI.evaluate [comb]
            print term
            print comb
            print result
        _ ->
            putStrLn "usage: cabal run -- subst | env | cps | secd | ski"
