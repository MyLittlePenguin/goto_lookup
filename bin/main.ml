open Goto_lookup

let ignore_case = ref false
let needles = ref []

let speclist =
  [ ("-i", Arg.Set ignore_case, "ignore case of needles and known entries") ]

let parse_arg needle = needles := needle :: !needles

let () =
  Arg.parse speclist parse_arg "usage: goto_lookup <search>";
  match List.rev !needles with
  | [] -> print_endline ""
  | [ needle ] -> (
      match find { ignore_case = !ignore_case; needle } lines with
      | None -> exit 404
      | Some p -> print_endline p)
  | needles -> (
      (* !query.needles |> Stringify.los_to_string |> print_endline ; *)
      match filter { ignore_case = !ignore_case; needles } lines with
      | [] -> exit 404
      | hd :: _ -> print_endline hd)
