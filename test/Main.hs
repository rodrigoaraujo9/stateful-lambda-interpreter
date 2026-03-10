module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Test.HUnit

import Ast
import Eval
import Lexer
import Parser

parseTerm :: String -> Term
parseTerm s = parse (lexer s)

evalNormal :: String -> Value
evalNormal s = eval (parseTerm s) []

evalCps :: String -> Value
evalCps s = eval_cps (parseTerm s) [] id

assertEvalBoth :: String -> String -> Value -> Assertion
assertEvalBoth label input expected = do
    let normal = evalNormal input
    let cps = evalCps input
    assertEqual (label ++ " normal") expected normal
    assertEqual (label ++ " cps") expected cps
    assertEqual (label ++ " normal and cps differ") normal cps

assertEvalErrorBoth :: String -> String -> Assertion
assertEvalErrorBoth label input = do
    normalRes <- try (evaluate (evalNormal input)) :: IO (Either ErrorCall Value)
    cpsRes <- try (evaluate (evalCps input)) :: IO (Either ErrorCall Value)

    case normalRes of
        Left _ -> pure ()
        Right v -> assertFailure (label ++ " normal should fail but got " ++ show v)

    case cpsRes of
        Left _ -> pure ()
        Right v -> assertFailure (label ++ " cps should fail but got " ++ show v)

test01 :: Test
test01 = TestCase $
    assertEvalBoth "const" "42" (Int 42)

test02 :: Test
test02 = TestCase $
    assertEvalBoth "addition" "1 + 2" (Int 3)

test03 :: Test
test03 = TestCase $
    assertEvalBoth "subtraction" "10 - 3" (Int 7)

test04 :: Test
test04 = TestCase $
    assertEvalBoth "multiplication" "6 * 7" (Int 42)

test05 :: Test
test05 = TestCase $
    assertEvalBoth "nested arithmetic" "(2 + 3) * (4 - 1)" (Int 15)

test06 :: Test
test06 = TestCase $
    assertEvalBoth "ifzero true" "if iszero 0 then 11 else 22" (Int 11)

test07 :: Test
test07 = TestCase $
    assertEvalBoth "ifzero false" "if iszero 5 then 11 else 22" (Int 22)

test08 :: Test
test08 = TestCase $
    assertEvalBoth "let binding" "let x = 5 in x + 1" (Int 6)

test09 :: Test
test09 = TestCase $
    assertEvalBoth "let shadowing" "let x = 2 in let x = 9 in x" (Int 9)

test10 :: Test
test10 = TestCase $
    assertEvalBoth "identity application" "(lambda x. x) 99" (Int 99)

test11 :: Test
test11 = TestCase $
    assertEvalBoth "lambda arithmetic" "(lambda x. x + 1) 5" (Int 6)

test12 :: Test
test12 = TestCase $
    assertEvalBoth "curried application" "((lambda x. lambda y. x + y) 4) 8" (Int 12)

test13 :: Test
test13 = TestCase $
    assertEvalBoth
        "closure capture"
        "let x = 10 in let f = lambda y. x + y in f 7"
        (Int 17)

test14 :: Test
test14 = TestCase $
    assertEvalBoth
        "lexical scoping"
        "let x = 10 in let f = lambda y. x + y in let x = 100 in f 1"
        (Int 11)

test15 :: Test
test15 = TestCase $
    assertEvalBoth
        "let evaluated value"
        "let x = 2 * 3 in x + 4"
        (Int 10)

test16 :: Test
test16 = TestCase $
    assertEvalErrorBoth "free variable" "x"

test17 :: Test
test17 = TestCase $
    assertEvalErrorBoth "apply non-closure" "3 4"

test18 :: Test
test18 = TestCase $
    assertEvalErrorBoth "plus type error" "(lambda x. x) + 1"

test19 :: Test
test19 = TestCase $
    assertEvalErrorBoth "minus type error" "10 - (lambda x. x)"

test20 :: Test
test20 = TestCase $
    assertEvalErrorBoth "times type error" "(lambda x. x) * (lambda y. y)"

test21 :: Test
test21 = TestCase $
    assertEvalErrorBoth "ifzero condition type error" "if iszero (lambda x. x) then 1 else 2"

test22 :: Test
test22 = TestCase $
    assertEvalErrorBoth "fix non-closure" "fix 5"

test23 :: Test
test23 = TestCase $
    assertEvalBoth
        "fix identity-like function"
        "(fix (lambda f. lambda x. x)) 33"
        (Int 33)

test24 :: Test
test24 = TestCase $
    assertEvalBoth
        "recursive sumdown"
        "let sumdown = fix (lambda f. lambda n. if iszero n then 0 else n + (f (n - 1))) in sumdown 4"
        (Int 10)

test25 :: Test
test25 = TestCase $
    assertEvalBoth
        "recursive multiplication"
        "let mul = fix (lambda self. lambda a. lambda b. if iszero b then 0 else a + ((self a) (b - 1))) in ((mul 3) 4)"
        (Int 12)

test26 :: Test
test26 = TestCase $
    assertEvalBoth
        "factorial"
        "let fact = fix (lambda f. lambda n. if iszero n then 1 else n * (f (n - 1))) in fact 5"
        (Int 120)

test27 :: Test
test27 = TestCase $
    assertEvalBoth
        "fibonacci"
        "let fib = fix (lambda f. lambda n. if iszero n then 0 else if iszero (n - 1) then 1 else (f (n - 1)) + (f (n - 2))) in fib 8"
        (Int 21)

test28 :: Test
test28 = TestCase $
    assertEvalBoth
        "closure plus recursion"
        "let base = 2 in let pow = fix (lambda self. lambda n. if iszero n then 1 else base * (self (n - 1))) in pow 5"
        (Int 32)

test29 :: Test
test29 = TestCase $
    assertEvalBoth
        "higher-order recursion"
        "let sumdown = fix (lambda f. lambda n. if iszero n then 0 else n + (f (n - 1))) in let twice = lambda g. lambda x. g (g x) in (twice sumdown) 2"
        (Int 6)

test30 :: Test
test30 = TestCase $
    assertEvalBoth
        "full combined program"
        "let fact = fix (lambda f. lambda n. if iszero n then 1 else n * (f (n - 1))) in let sumdown = fix (lambda g. lambda n. if iszero n then 0 else n + (g (n - 1))) in (fact 4) + (sumdown 5)"
        (Int 39)

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
    , TestLabel "22 fix non-closure error" test22
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
        then error "Some eval tests failed."
        else putStrLn "All 30 eval tests passed."
