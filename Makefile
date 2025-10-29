build:
	dune build

install: build
	dune install

.PHONY: test
test:
	dune test

