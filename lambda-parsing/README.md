# Monadic parsing for augmented lambda expressions

## Supported syntax

- Functional expressions: `\f x. f(x x)`. Similar to lambda calculus, such expressions are parenthesed to the right i.e. `\f . \x . f (x x)` works as expected. There is also an equivalent and more eye-appealing version `fun f x -> f (x x)`

- Applications: as above, inspired by conventions from lambda calculus, applications are parenthesed to the left i.e. `f a b c = ((f a) b) c`

- Let expressions: for example `let t = f (x x) in g t`. We do not care about the type polymorphism, hence above is equivalent with `(\t . g t) (f (x x))` (arbeit probably more readable for larger expressions)

- Constants: numbers `0, -213, 117`, strings `"abc", ""`, chars `'x'` along with basic operators like `+ , -, ++, :` 

- Comments: look like this `let x = a b // hello there` and lasts to the end of the line 

- Pattern maching: syntax for tuples is supported `<a, f x, x>`. You can use pattern matching in function argument to get access to primitive fields e.g. `(\ <x, y> z . x y z) <f x, g y> (h z)`

## Usage

Just run `make run` in the command line. Input is in the `input.in` file.

