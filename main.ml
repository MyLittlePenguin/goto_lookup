open Goto_lookup

type flags = ALL | IGNORE_CASE

let query = ref { ignore_case = false; needles = [] }

let parse_arg = function
  | "-i" -> query := { !query with ignore_case = true }
  | needle -> query := { !query with needles = needle :: !query.needles }

let () =
  Arg.parse []
    parse_arg
    (* (fun it -> needles := String.trim it :: !needles) *)
    "usage: goto_lookup <search>";
  match List.rev !query.needles with
  | [] -> print_endline ""
  | [ needle ] -> (
      match find needle lines with
      | None -> exit 404
      | Some p -> print_endline p)
  | needles -> (
      (* needles |> los_to_string |> print_endline ; *)
      match filter needles lines with
      | [] -> exit 404
      | hd :: _ -> print_endline hd)
