open Goto_lookup

(* type flags = ALL | IGNORE_CASE *)

(* let query = ref { ignore_case = false; needles = [] } *)
let ignore_case = ref false
let needles = ref []

let speclist =
  [ ("-i", Arg.Set ignore_case, "ignore case of needles and known entries") ]

let parse_arg needle =
  (* | "-i" -> query := { !query with ignore_case = true } *)
  (* | needle -> query := { !query with needles = needle :: !query.needles } *)
  needles := needle :: !needles

let () =
  Arg.parse speclist parse_arg
    (* (fun it -> needles := String.trim it :: !needles) *)
    "usage: goto_lookup <search>";
  match List.rev !needles with
  | [] -> print_endline ""
  | [ needle ] -> (
      match find needle lines with
      | None -> exit 404
      | Some p -> print_endline p)
  | needles -> (
      (* !query.needles |> Stringify.los_to_string |> print_endline ; *)
      (* match filter !query lines with *)
      match filter { ignore_case = !ignore_case; needles } lines with
      | [] -> exit 404
      | hd :: _ -> print_endline hd)
