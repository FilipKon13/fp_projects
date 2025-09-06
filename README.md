# Functional Programming Projects

This repository is a collection of projects written in Haskell and OCaml.

## Projects

### 1. [Tagless-Final Key-Value Store](./tagless-final/)

*   **Language**: Haskell
*   **Subject**: The Tagless-Final pattern

This project demonstrates how to build a polymorphic API that can be interpreted by multiple backends without changing the core logic. It defines a simple key-value store interface and provides three distinct interpreters:
1.  An **HTTP backend** communicating with a Python server.
2.  A low-level **RPC backend** communicating with a C server.
3.  An **in-memory test backend** for unit testing.

[**Read more...**](./tagless-final/README.md)

---

### 2. [Monadic Parser for an Augmented Lambda Calculus](./lambda-parsing/)

*   **Language**: OCaml
*   **Subject**: Monadic parsing

This project implements a parser for an extended lambda calculus from scratch using monadic parser combinators. The language supports features like `let` bindings, integers, tuples, and pattern matching. An accompanying simple evaluator is included to test the parser's output.

[**Read more...**](./lambda-parsing/README.md)

---

### 3. Hindley-Milner Type Inference

*   **Language**: OCaml
*   **Subject**: Hindley-Milner (Algorithm W) Type Inference

This project is a complete implementation of the Hindley-Milner type inference algorithm for a small functional language. It explores three different implementation strategies for extending the core algorithm to support features like:
*   Let-polymorphism
*   Recursive bindings (`let rec`)
*   Lists, integers, and boolean logic

[**Read more...**](./hindley-milner/README.md)

---



## Building and Running

Each project is self-contained and has its own build and execution instructions. Please navigate to the respective project directory and consult its `README.md` file for details.

Test suites are executed as of the CI pipeline.
