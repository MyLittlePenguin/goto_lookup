.PHONY: build
build:
	dune build

.PHONY: install
install: build
	dune install

.PHONY: test
test:
	dune test

