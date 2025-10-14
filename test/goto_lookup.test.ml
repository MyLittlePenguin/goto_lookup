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
    assert_equals id (substr_after "c" "ocaml") "aml";
    assert_equals id (substr_after "ca" "ocaml") "ml";
    assert_equals id (substr_after "z" "ocaml") "";
    assert_equals los_to_string
      (filter [ "oc" ]
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo"; "Software/ocaml" ];
    assert_equals los_to_string
      (filter [ "ocaml" ]
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo"; "Software/ocaml" ];
    assert_equals los_to_string
      (filter [ "oc"; "bi" ]
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "workspace/ocaml/bingo" ];
    assert_equals los_to_string
      (filter [ "oc"; "bu" ]
         [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [];
    assert_equals los_to_string
      (filter [] [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ];
    assert_equals los_to_string
      (filter []
         [ ""; "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
      [ ""; "asdf/qwerty"; "workspace/ocaml/bingo"; "Software/ocaml" ])
