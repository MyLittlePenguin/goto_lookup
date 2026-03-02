let version_file = "version.txt"
let add_newline s = s ^ "\n"

let rec run = function
  | "" | "build" | "compile" ->
    echo "build";
    run "update_version";
    cmd "dune build"

  | "release" ->
    echo "release";
    run "create_version";
    cmd "dune build --profile=release"

  | "run" ->
    run "create_version";
    cmd "dune exec -- goto_lookup --list"

  | "version" ->
    cmd "cat version.txt"

  | "install" ->
    run "release";
    echo "install";
    cmd "dune install"

  | "test" ->
    cmd "dune test";
    echo "Tests successful"

  | "help" ->
    cmd "dune exec -- goto_lookup --help"

  | "create_version" ->
    write "bin/version.ml" @@ String.concat "" [
      "let number = \"";
      version_file |> read |> String.trim;
      "\"\n"
    ]
  | "cross-win" ->
    echo "cross compile windows";
    cmd "ocamlfind -toolchain windows ocamlopt -o windows/goto_lookup.o -c lib/goto_lookup/goto_lookup.ml";
    cmd "ocamlfind -toolchain windows ocamlopt -o windows/errors.o -c lib/errors/errors.ml";
    cmd "ocamlfind -toolchain windows ocamlopt -o windows/stringify.o -c lib/stringify/stringify.ml";
    cmd "ocamlfind -toolchain windows ocamlopt -o windows/lookup windows/goto_lookup.cmx windows/stringify.cmx windows/errors.cmx"

  | "update_version" ->
    let version = String.trim @@ read version_file in
    let parts = String.split_on_char '.' version |> Array.of_list in
    let new_version = String.concat "." [
      parts.(0);
      parts.(1);
      parts.(2)
        |> int_of_string
        |> (+) 1
        |> string_of_int
        |> add_newline
    ] in
    write version_file new_version;
    run "create_version";

  | _ -> exit 404
