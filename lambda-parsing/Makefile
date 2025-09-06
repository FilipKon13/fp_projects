TEST_FILES := $(wildcard tests/*.in) $(wildcard files/*.in)

all: main

test: main
	@echo "Running tests..."
	@for test_file in $(TEST_FILES); do \
		echo "--- Running test: $$test_file ---"; \
		if ! ./main < $$test_file; then \
			echo "\n--- Test failed: $$test_file ---"; \
			exit 1; \
		fi; \
		echo; \
	done
	@echo "All tests passed."

main: src/expression.ml src/token.ml src/monad.ml src/tokenization.ml src/parsing.ml src/eval.ml src/main.ml
	ocamlc -g -I src unix.cma $^ -o main

run: main
	./main

clean:
	rm -f main src/*.cmi src/*.cmo test.log

.PHONY: run clean test test-one

test-one: main
ifndef test
	@echo "Usage: make test-one test=<path/to/test.in>"
	@exit 1
endif
	@echo "--- Running single test: $(test) ---"
	@./main < $(test)
