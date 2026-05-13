open Stringify

exception AssertionFailure of string

let fail msg = raise @@ AssertionFailure msg
let eq a b = a = b
let assert_fn (fn : 'a -> 'b -> bool) a b msg = if fn a b then () else fail msg

let assert_equals (fn : 'a -> string) a b =
  let a_str = fn a in
  let b_str = fn b in
  assert_fn eq a b (a_str ^ " <> " ^ b_str)

let () =
  Lookup.(
    let test =
      assert_equals (function
        | Some x -> "Some " ^ string_of_int x
        | None -> "None")
    in
    test (index_of "oc" "ocaml") (Some 0);
    test (index_of "ml" "ocaml") (Some 3);
    let test = assert_equals id in
    test (substr_after "c" "ocaml") "aml";
    test (substr_after "ca" "ocaml") "ml";
    test (substr_after "z" "ocaml") "";
    test (relativ_to_abs "a/b/../b") "a/b";
    test (relativ_to_abs "/a/b/../b") "/a/b";
    test (relativ_to_abs "a/b/./c") "a/b/c";
    test (relativ_to_abs "../b/./c") "b/c";
    test (relativ_to_abs "./b/./c") "b/c";
    test (relativ_to_abs "a/b/c/..") "a/b";
    let test = assert_equals (function true -> "true" | false -> "false") in
    test (contains "ca" "ocaml") true;
    test (contains "z" "ocaml") false;
    test (contains "o" "ocaml") true;
    test (contains "l" "ocaml") true;
    let test = assert_equals los_to_string in
    test
      (filter
         (Multi { ignore_case = false; needles = [ "oc" ] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         (Multi { ignore_case = false; needles = [ "ocaml" ] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         (Multi { ignore_case = false; needles = [ "oc"; "bi" ] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo" ];
    test
      (filter
         (Multi { ignore_case = false; needles = [ "oc"; "bu" ] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [];
    test
      (filter
         (Multi { ignore_case = false; needles = [] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         (Multi { ignore_case = false; needles = [] })
         [ ""; "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ ""; "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         (Multi { ignore_case = false; needles = [ "software" ] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [];
    test
      (filter
         (Multi { ignore_case = true; needles = [ "software" ] })
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "Software/ocaml" ])
