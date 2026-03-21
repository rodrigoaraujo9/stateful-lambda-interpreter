module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Test.HUnit

import qualified Ast
import Ast (Term(..))
import qualified Types as S

import Lexer
import Parser
import Run

import qualified EvalEnv as Env

import qualified EvalSubst as Subst

import qualified EvalCPS as CPS

import qualified Combinator as SKI

import qualified CompilerSECD as SECD

data Backend
    = BSubst
    | BEnv
    | BCps
    | BSecd
    | BSki
    deriving (Eq, Show)

data Result
    = RInt Int
    | RClosure
    | RLambda
    deriving (Eq, Show)

parseTerm :: String -> Term
parseTerm s = parse (lexer s)

runBackend :: Backend -> String -> Result
runBackend backend input =
    case backend of
        BSubst ->
            normalizeTerm (Subst.eval term)

        BEnv ->
            normalizeAstValue (Env.eval term [])

        BCps ->
            normalizeAstValue (CPS.eval term [] id)

        BSecd ->
            normalizeSecdValue (run (SECD.compile term []))

        BSki ->
            normalizeSkiValue (SKI.evaluate [SKI.compile term])
  where
    term = parseTerm input

normalizeSkiValue :: [SKI.Comb] -> Result
normalizeSkiValue [c] =
    case c of
        SKI.V _ -> error "SKI result is a free variable"
        _       -> RClosure
normalizeSkiValue _ = error "SKI backend did not normalize to a single combinator"

normalizeAstValue :: Ast.Value -> Result
normalizeAstValue v =
    case v of
        Ast.Int n      -> RInt n
        Ast.Closure _ _ -> RClosure

normalizeSecdValue :: S.Value -> Result
normalizeSecdValue v =
    case v of
        S.I n -> RInt n
        S.A _ -> RClosure

assertRunAll :: String -> String -> Result -> Assertion
assertRunAll label input expected =
    mapM_ check [BSubst, BEnv, BCps, BSecd]
  where
    check backend = do
        let result = runBackend backend input
        assertEqual (label ++ " [" ++ show backend ++ "]") expected result

assertRunErrorAll :: String -> String -> Assertion
assertRunErrorAll label input =
    mapM_ check [BSubst, BEnv, BCps, BSecd]
  where
    check backend = do
        res <- try (evaluate (runBackend backend input)) :: IO (Either ErrorCall Result)
        case res of
            Left _  -> pure ()
            Right v ->
                assertFailure (label ++ " [" ++ show backend ++ "] should fail but got " ++ show v)

test01 :: Test
test01 = TestCase $
    assertRunAll "const" "42" (RInt 42)

test02 :: Test
test02 = TestCase $
    assertRunAll "addition" "1 + 2" (RInt 3)

test03 :: Test
test03 = TestCase $
    assertRunAll "subtraction" "10 - 3" (RInt 7)

test04 :: Test
test04 = TestCase $
    assertRunAll "multiplication" "6 * 7" (RInt 42)

test05 :: Test
test05 = TestCase $
    assertRunAll "nested arithmetic" "(2 + 3) * (4 - 1)" (RInt 15)

test06 :: Test
test06 = TestCase $
    assertRunAll "ifzero true" "if iszero 0 then 11 else 22" (RInt 11)

test07 :: Test
test07 = TestCase $
    assertRunAll "ifzero false" "if iszero 5 then 11 else 22" (RInt 22)

test08 :: Test
test08 = TestCase $
    assertRunAll "let binding" "let x = 5 in x + 1" (RInt 6)

test09 :: Test
test09 = TestCase $
    assertRunAll "let shadowing" "let x = 2 in let x = 9 in x" (RInt 9)

test10 :: Test
test10 = TestCase $
    assertRunAll "identity application" "(lambda x. x) 99" (RInt 99)

test11 :: Test
test11 = TestCase $
    assertRunAll "lambda arithmetic" "(lambda x. x + 1) 5" (RInt 6)

test12 :: Test
test12 = TestCase $
    assertRunAll "curried application" "((lambda x. lambda y. x + y) 4) 8" (RInt 12)

test13 :: Test
test13 = TestCase $
    assertRunAll
        "closure capture"
        "let x = 10 in let f = lambda y. x + y in f 7"
        (RInt 17)

test14 :: Test
test14 = TestCase $
    assertRunAll
        "lexical scoping"
        "let x = 10 in let f = lambda y. x + y in let x = 100 in f 1"
        (RInt 11)

test15 :: Test
test15 = TestCase $
    assertRunAll
        "let evaluated value"
        "let x = 2 * 3 in x + 4"
        (RInt 10)

test16 :: Test
test16 = TestCase $
    assertRunErrorAll "free variable" "x"

test17 :: Test
test17 = TestCase $
    assertRunErrorAll "apply non-closure" "3 4"

test18 :: Test
test18 = TestCase $
    assertRunErrorAll "plus type error" "(lambda x. x) + 1"

test19 :: Test
test19 = TestCase $
    assertRunErrorAll "minus type error" "10 - (lambda x. x)"

test20 :: Test
test20 = TestCase $
    assertRunErrorAll "times type error" "(lambda x. x) * (lambda y. y)"

test21 :: Test
test21 = TestCase $
    assertRunErrorAll "ifzero condition type error" "if iszero (lambda x. x) then 1 else 2"

test22 :: Test
test22 = TestCase $
    assertRunErrorAll "fix non-function shape" "fix 5"

test23 :: Test
test23 = TestCase $
    assertRunAll
        "fix identity-like function"
        "(fix (lambda f. lambda x. x)) 33"
        (RInt 33)

test24 :: Test
test24 = TestCase $
    assertRunAll
        "recursive sumdown"
        "let sumdown = fix (lambda f. lambda n. if iszero n then 0 else n + (f (n - 1))) in sumdown 4"
        (RInt 10)

test25 :: Test
test25 = TestCase $
    assertRunAll
        "recursive multiplication"
        "let mul = fix (lambda self. lambda a. lambda b. if iszero b then 0 else a + ((self a) (b - 1))) in ((mul 3) 4)"
        (RInt 12)

test26 :: Test
test26 = TestCase $
    assertRunAll
        "factorial"
        "let fact = fix (lambda f. lambda n. if iszero n then 1 else n * (f (n - 1))) in fact 5"
        (RInt 120)

test27 :: Test
test27 = TestCase $
    assertRunAll
        "fibonacci"
        "let fib = fix (lambda f. lambda n. if iszero n then 0 else if iszero (n - 1) then 1 else (f (n - 1)) + (f (n - 2))) in fib 8"
        (RInt 21)

test28 :: Test
test28 = TestCase $
    assertRunAll
        "closure plus recursion"
        "let base = 2 in let pow = fix (lambda self. lambda n. if iszero n then 1 else base * (self (n - 1))) in pow 5"
        (RInt 32)

test29 :: Test
test29 = TestCase $
    assertRunAll
        "higher-order recursion"
        "let sumdown = fix (lambda f. lambda n. if iszero n then 0 else n + (f (n - 1))) in let twice = lambda g. lambda x. g (g x) in (twice sumdown) 2"
        (RInt 6)

test30 :: Test
test30 = TestCase $
    assertRunAll
        "full combined program"
        "let fact = fix (lambda f. lambda n. if iszero n then 1 else n * (f (n - 1))) in let sumdown = fix (lambda g. lambda n. if iszero n then 0 else n + (g (n - 1))) in (fact 4) + (sumdown 5)"
        (RInt 39)

tests :: Test
tests = TestList
    [ TestLabel "01 const" test01
    , TestLabel "02 addition" test02
    , TestLabel "03 subtraction" test03
    , TestLabel "04 multiplication" test04
    , TestLabel "05 nested arithmetic" test05
    , TestLabel "06 ifzero true" test06
    , TestLabel "07 ifzero false" test07
    , TestLabel "08 let binding" test08
    , TestLabel "09 let shadowing" test09
    , TestLabel "10 identity application" test10
    , TestLabel "11 lambda arithmetic" test11
    , TestLabel "12 curried application" test12
    , TestLabel "13 closure capture" test13
    , TestLabel "14 lexical scoping" test14
    , TestLabel "15 let evaluated value" test15
    , TestLabel "16 free variable error" test16
    , TestLabel "17 apply non-closure error" test17
    , TestLabel "18 plus type error" test18
    , TestLabel "19 minus type error" test19
    , TestLabel "20 times type error" test20
    , TestLabel "21 ifzero type error" test21
    , TestLabel "22 fix shape error" test22
    , TestLabel "23 fix simple function" test23
    , TestLabel "24 recursive sumdown" test24
    , TestLabel "25 recursive mul" test25
    , TestLabel "26 factorial" test26
    , TestLabel "27 fibonacci" test27
    , TestLabel "28 closure plus recursion" test28
    , TestLabel "29 higher-order recursion" test29
    , TestLabel "30 full combined program" test30
    ]

main :: IO ()
main = do
    results <- runTestTT tests
    if failures results > 0 || errors results > 0
        then error "Some backend tests failed."
        else putStrLn "All tests passed for subst, env, cps, and secd."
