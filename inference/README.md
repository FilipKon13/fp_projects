# Hindley-Milner Type Inference in OCaml

This project is a complete implementation of the Hindley-Milner (Algorithm W) type inference system for a small functional language, written in OCaml. It can automatically deduce the types of expressions, supporting key features like let-polymorphism and recursive bindings.

## Comparison of three approaches

This project explores three different implementations of the Hindley-Milner algorithm and its extensions: `base`, `rules`, and `context`.

### 1. `base` - Basic Algorithm W

This is a minimal implementation. It supports only canonical lambda calculus with let-polymorphism.

### 2. `rules` - Declarative approach for language extensions

In this version to add new features into the language, we expand the set of inference rules. This results in the modyfications of the main type-inference algorithm in file `inference.ml`.

### 3. `context` - Context-based approach for language extensions

This is the most feature-complete and robust implementation. It uses a non-empty default typing context to provide new features. Because of that, we can keep the main inference algorithm same, and new features are added as new entries in the default context.

This implementation supports most features:

- Lists - `Cons` and `ListNil`,
- Recursive let - `let rec` construction is supported via addition of `fix` operator of type `∀a.((a => a) => a)`. An interesting consequence of this addition is a possibility to inhabit types which do not correspond with any theorem of classical logic e.g. an expression `(λa. (let rec x = (a x) in x))` has type `((a -> a) -> a)`,
- Bool and Int literals,
- Basic arithmetic - by adding arithmetic operators of appropriate types,
- If-then-else construct - by adding `ite` operator of type `∀a.(TBool => (a => (a => a)))`.

## Building and Testing

### Prerequisites

One can install the necessary dependencies with:

```bash
sudo apt-get update
sudo apt-get install -y build-essential opam
# Setup for opam and OCaml
opam init
opam install ocaml
```

### Execution

Run the test suite.

```bash
# Build the project and run the test suite
make test
```

The command runs the type checker against a suite of examples in `examples.ml`, which includes both expressions that should type-check successfully and those that are designed to fail.
