# Monadic Parser for an Augmented Lambda Calculus

This project implements a parser for a extension of the lambda calculus, It supports features like `let` bindings, integers, and tuples. The parser is built in OCaml using monadic parser combinators.

The primary focus of this project is the **parser implementation**. An accompanying evaluator (`eval`) is provided, but its main purpose is to serve as a simple tool to test the parser's correctness by executing the generated abstract syntax trees. It is not an optimized interpreter.

## Supported syntax

The parser supports a syntax that blends conventions from lambda calculus and ML-family languages.

### Lambda Abstractions

Functions can be defined using either lambda calculus-style or `fun` keyword syntax.

-   `\x y z. body`
-   `fun x y z -> body`

Abstractions are **right-associative**. For example, `fun f x -> f x` is parsed as `fun f -> (fun x -> f x)`.

### Function Application

Function application is written by placing expressions next to each other.

-   `f x y`

Application is **left-associative**. For example, `f a b c` is parsed as `((f a) b) c`. Parentheses can be used to override precedence: `f (g x)`.

### Let Bindings

`let` expressions are used to introduce local bindings.

-   `let x = e1 in e2`

As we do not care about let-polymorphism or typing, this is just a syntactic sugar for `(\x. e2) e1`. The scope of `x` is the expression `e2`.

**Shadowing**: The language supports variable shadowing. A new binding for a variable will hide any existing binding of the same name in the current scope.
```
let x = 10 in
  let x = 20 in // This x shadows the outer x
    x + x        // Evaluates to 40
```

### Literals and Operators

-   **Integers**: `0`, `117`, `42`
-   **Binary Operators**: `+`, `-`, `*`, `==`. The `==` operator returns `1` is its arguments are equal and `0` otherwise.

### Conditionals

A simple if-then-else construct is provided with the `ite` (if-then-else) keyword.

-   `ite condition then_branch else_branch`

The `condition` is evaluated. If it results is non-zero, the `then_branch` is evaluated. Otherwise, the `else_branch` is evaluated.

### Tuples and Pattern Matching

The language supports tuples and pattern matching in function arguments.

-   **Tuple Creation**: `<e1, e2, e3>`
-   **Pattern Matching**: A function can deconstruct a tuple argument directly.
    ```
    // Defines a function that takes a 2-tuple and returns its first element
    let fst = fun <x, y> -> x in
    
    // Applies the function
    fst <10, 20> // Evaluates to 10
    ```

### Comments

Single-line comments start with `//` and extend to the end of the line.

-   `let x = 1 // This is a comment`


## Usage

To run the parser and evaluator on a sample input:

```bash
make run
```

