(* #load "goto_lookup.cmo" *)
let id x = x

let l_to_string fn list =
  let rec aux acc = function
    | [] -> acc ^ "]"
    | a :: [] -> acc ^ fn a ^ "]"
    | a :: b -> aux (acc ^ fn a ^ "; ") b
  in
  aux "[" list

let los_to_string list = l_to_string id list

let () =
  (* let input = String.trim In_channel.(input_all stdin) in *)
  let needles = ref [] in
  Arg.parse []
    (fun it -> needles := String.trim it :: !needles)
    "usage: goto_lookup <search>";
  match List.rev !needles with
  | [] -> print_endline ""
  | [ needle ] -> (
      match Goto_lookup.(find needle lines) with
      | None -> exit 404
      | Some p -> print_endline p)
  | needles -> (
    (* needles |> los_to_string |> print_endline ; *)
      match Goto_lookup.(filter needles lines) with
      | [] -> exit 404
      | hd :: _ -> print_endline hd)
