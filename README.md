# stateful-lambda-interpreter

A lambda calculus interpreter written in Haskell for a programming languages course.
The abstract syntax tree and the evaluation model were provided by the teacher, and the interpreter is implemented using an explicit runtime environment with closures.

## What it does

Parses and evaluates expressions with support for:

- Variables and lambda abstractions (`\x. x` or `lambda x. x`)
- Function application (`f x`)
- Integer constants and arithmetic (`+`, `-`, `*`)
- Conditionals (`if iszero e then e else e`)
- Local definitions (`let x = e in e`)
- Fixed-point operator (`fix`)

## Evaluation model

The interpreter evaluates terms with the function:

```haskell
eval :: Term -> Env -> Value
```

where:

- `Term` is the source language expression
- `Env` is the runtime environment, storing variable bindings
- `Value` is the result of evaluation

### Runtime values

```haskell
type Env = [(Ident, Value)]

data Value
  = Int Int
  | Closure Term Env
```

This means evaluation produces either:

- an integer value
- a closure, which stores:
    - the lambda term itself
    - the environment where it was created

## Evaluation strategy

- **Applicative order** — function arguments are evaluated before application
- **Weak evaluation of lambdas** — lambda bodies are not reduced until application
- **Environment-based semantics** — variables are resolved through the runtime environment
- **Closures** are used to preserve lexical scope
- **No free variables allowed at runtime** — a free variable occurrence raises an error

## Supported constructs

### Variables

Variables are looked up in the current environment.

### Lambda abstractions

A lambda evaluates to a closure containing the lambda term and its defining environment.

### Application

To evaluate `e1 e2`:

1. evaluate `e1` to a closure
2. evaluate `e2` to a value
3. extend the closure environment with the argument binding
4. evaluate the lambda body in that extended environment

### Arithmetic

`+`, `-`, and `*` require both operands to evaluate to integers.

### Conditionals

`if iszero e1 then e2 else e3`

- if `e1` evaluates to `Int 0`, evaluate `e2`
- otherwise, if it evaluates to `Int n`, evaluate `e3`

### Let

`let x = e1 in e2` is evaluated as syntactic sugar for function application:
`(App (Lambda x e2) e1)`


### Fix

`fix` is used for recursive definitions by building a self-referential closure.

## How to run

You need:

- GHC
- Cabal
- Happy

Generate the parser and run the interpreter with:

```bash
happy src/Parser.y
cabal run
```

Then type an expression and finish input with `Ctrl+D`.

## Examples

```txt
if iszero 0 then 99 else 5
-- Int 99

let twice = lambda f . lambda x . f (f x) in twice (lambda x . x + 1) 42
-- Int 44

let fact = fix lambda f . lambda n . if iszero n then 1 else n * f (n - 1) in fact 10
-- Int 3628800
```
