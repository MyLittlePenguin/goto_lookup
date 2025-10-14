.PHONY: build
build: build_goto_lookup build_main
	ocamlopt -I +unix unix.cmxa -I bin goto_lookup.cmx main.cmx -o bin/goto_lookup

.PHONY: clean_build
clean_build: clean build

.PHONY: build_goto_lookup
build_goto_lookup:
	ocamlopt -I +unix unix.cmxa -o bin/goto_lookup.cmx -c src/goto_lookup.ml

.PHONY: build_main
build_main:
	ocamlopt -I +unix unix.cmxa -I bin goto_lookup.cmx -o bin/main.cmx -c src/main.ml

.PHONY: test
test:
	ocamlc -I +unix unix.cma -o test/out/goto_lookup.cmo -c src/goto_lookup.ml
	ocamlc -I +unix unix.cma -o test/out/stringify.cmo -c src/stringify.ml
	ocaml -I +unix unix.cma -I test/out stringify.cmo goto_lookup.cmo test/goto_lookup.test.ml

.PHONY: clean
clean:
	rm -rf bin/*
