<br />
<div align="center">
  <h3 align="center">lambda-calculus</h3>
  <p align="center">
    A lambda calculus interpreter, compiler, and SECD virtual machine in Haskell.
  </p>
</div>

## About

This project was developed for a programming languages course and explores multiple ways of executing the same lambda calculus language inside a single codebase.

The goal was not only to evaluate expressions, but to understand how different semantic models relate to each other in practice: direct substitution, closure-based evaluation, continuation-passing style, and finally compilation into an abstract machine.

The parsing infrastructure and abstract syntax tree were provided in class, while the interpreters, compiler, runtime machine, and test suite were implemented independently.

The language currently supports:

- variables and lambda abstractions
- function application
- integer arithmetic
- conditionals with `if iszero`
- local definitions with `let`
- recursion through `fix`

## Execution models

The executable supports four execution modes, each exposing a different semantic perspective over the same language.

### Substitution evaluator

Direct beta-reduction through substitution.

Focuses on the most immediate operational interpretation of lambda calculus:

- applicative order
- weak normal form
- explicit substitution

```bash
cabal run exe:lambda-calculus -- subst
```

### Environment evaluator

Evaluation through closures and runtime environments.

Instead of rewriting syntax, values are carried through lexical environments:

- applicative order
- lexical scoping
- closures preserve defining environments

```bash
cabal run exe:lambda-calculus -- env
```

### CPS evaluator

A continuation-passing version of the environment evaluator.

This makes control flow explicit and mirrors the same semantics under continuation transformation:

- explicit continuations
- same observable results as environment evaluation
- explicit control transfer

```bash
cabal run exe:lambda-calculus -- cps
```

### SECD machine

Expressions are compiled into instructions and executed by a stateful SECD machine.

This moves from interpretation into explicit machine execution:

- stack-based execution
- explicit environments
- closure store
- dump-based returns

```bash
cabal run exe:lambda-calculus -- secd
```

## Machine model

The SECD implementation follows the abstract machine studied in class:

- **S** → stack
- **E** → environment
- **C** → control
- **D** → dump
- **Store** → closure memory

Machine configuration:

```haskell
type Conf = (Stack, Env, Code, Dump, Store)
```

Closures are stored explicitly:

```haskell
type Closure = (Code, Env)
```

Runtime values:

```haskell
data Value
  = I Int
  | A Addr
```

## Compilation

Source terms are compiled into machine instructions:

```haskell
compile :: Term -> [Ident] -> Code
```

Variables are translated into lexical indices before execution.

Example:

```txt
lambda x . x + 1
```

becomes:

```haskell
[LDF [LD 0, LDC 1, ADD, RTN]]
```

## Supported instructions

The SECD runtime currently supports:

```haskell
LDC
LD
ADD
SUB
MUL
LDF
LDRF
AP
RTN
SEL
JOIN
HALT
```

Recursive closures are handled through `LDRF`, allowing cyclic store allocation for `fix`.

## How to run

Generate the parser and build:

```bash
happy src/Parser.y
cabal build
```

Then choose a backend:

```bash
cabal run exe:lambda-calculus -- subst
cabal run exe:lambda-calculus -- env
cabal run exe:lambda-calculus -- cps
cabal run exe:lambda-calculus -- secd
```

Input is read from standard input.

## Testing

The project includes a shared automated suite covering all execution models:

```bash
cabal test
```

This verifies:

- arithmetic
- lexical scoping
- closures
- recursion
- higher-order functions
- machine/runtime errors

## Example programs

```txt
if iszero 0 then 99 else 5
```

```txt
let twice = lambda f . lambda x . f (f x) in twice (lambda x . x + 1) 42
```

```txt
let fact = fix lambda f . lambda n . if iszero n then 1 else n * f (n - 1) in fact 10
```
