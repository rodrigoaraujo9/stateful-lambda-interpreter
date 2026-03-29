<br />
<div align="center">
  <h3 align="center">lambda-calculus</h3>
  <p align="center">
    A collection of lambda calculus interpreters and compilers to SECD and Extended SKI in Haskell.
  </p>
</div>

## About

This project was developed for a programming languages course and explores programming languages, diving into multiple strategies for evaluation and intermediary code representations.

The goal was not only to evaluate expressions, but to understand how different semantic models relate to each other in practice: direct substitution, closure-based evaluation, continuation-passing style, compilation into an abstract machine and graph based reduction.

The abstract syntax tree for the fun languages was provided in class, while the interpreters, compilers, the lexer and the parser were implemented independently.

The language currently supports:

- variables and lambda abstractions
- function application
- integer arithmetic
- conditionals with `if iszero`
- local definitions with `let`
- recursion through `fix`

### Substitution evaluator

Direct beta-reduction through substitution.

Focuses on the most immediate operational interpretation of lambda calculus:

- applicative order
- weak normal form
- explicit substitution

```bash
cd interpreters
cabal run exe:lambda-calculus -- subst
```

### Environment evaluator

Evaluation through closures and runtime environments.

Instead of rewriting syntax, values are carried through lexical environments:

- applicative order
- lexical scoping
- closures preserve defining environments

```bash
cd interpreters

cabal run exe:lambda-calculus -- env
```

### CPS evaluator

A continuation-passing version of the environment evaluator.

This makes control flow explicit and mirrors the same semantics under continuation transformation:

- explicit continuations
- same observable results as environment evaluation
- explicit control transfer

```bash
cd interpreters
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
cd compilers
cabal run exe:fun test.fun
```

### G-Machine

Although not yet a fully implemented G-Machine, it does compilation into Extended SKI combinators followed by weak reduction.

This evaluator moves from lambda terms to a combinatory representation, eliminating variable binding and executing programs through combinator rewriting.

It follows the graph-reduction and combinatory logic model presented in the course slides:

- weak head normal form evaluation
- non-strict execution
- optimized bracket abstraction
- Extended SKI with `S`, `K`, `I`, `B`, `C` and `Y`
- strict handling of primitive arithmetic and `ifzero`

```bash
cd compilers/g-machine
cabal run exe:fun test.fun
```
