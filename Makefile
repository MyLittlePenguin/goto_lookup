.PHONY: build
build:
	nix-shell -p ocaml --run "ocamlopt -I +unix unix.cmxa -c goto_lookup.ml"
	nix-shell -p ocaml --run "ocamlopt -I +unix unix.cmxa goto_lookup.cmx -o goto_lookup main.ml"

.PHONY: run
run:
	nix-shell -p ocaml --run "ocaml -I +unix unix.cma goto_lookup.ml"

.PHONY: test
test:
	nix-shell -p ocaml --run "ocamlc -I +unix unix.cma -c goto_lookup.ml"
	nix-shell -p ocaml --run "ocaml -I +unix unix.cma goto_lookup.cmo goto_lookup.test.ml"

.PHONY: clean
clean:
	rm goto_lookup goto_lookup.cmi goto_lookup.cmo goto_lookup.cmx goto_lookup.o
