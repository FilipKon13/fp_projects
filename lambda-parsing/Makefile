test: run

main: src/expression.ml src/token.ml src/monad.ml src/tokenization.ml src/parsing.ml src/eval.ml src/main.ml
	ocamlc -g -I src unix.cma $^ -o main

run: main
	./main < input.in

clean:
	rm -f main src/*.cmi src/*.cmo

.PHONY: run clean test
