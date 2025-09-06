all: run

main: types.ml examples.ml utils.ml inference.ml main.ml
	ocamlc $^ unix.cma -o main

run: main
	./main

clean:
	rm -f main *.cmi *.cmo

.PHONY: run clean
