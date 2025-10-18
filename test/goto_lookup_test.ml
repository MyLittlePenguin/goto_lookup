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
  Goto_lookup.(
    let test = assert_equals id in
    test (substr_after "c" "ocaml") "aml";
    test (substr_after "ca" "ocaml") "ml";
    test (substr_after "z" "ocaml") "";
    let test = assert_equals los_to_string in
    test
      (filter
         { ignore_case = false; needles = [ "oc" ] }
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         { ignore_case = false; needles = [ "ocaml" ] }
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         { ignore_case = false; needles = [ "oc"; "bi" ] }
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo" ];
    test
      (filter
         { ignore_case = false; needles = [ "oc"; "bu" ] }
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [];
    test
      (filter
         { ignore_case = false; needles = [] }
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ];
    test
      (filter
         { ignore_case = false; needles = [] }
         [ ""; "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ ""; "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
