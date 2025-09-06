all: build

build:
	make -C base main
	make -C context main
	make -C rules main

test: build
	make -C base run
	make -C context run
	make -C rules run

clean:
	make -C base clean
	make -C context clean
	make -C rules clean

.PHONY: build clean test
