open Goto_lookup

type action_type = Print_Version | Lookup

let ignore_case = ref false
let needles = ref []
let action = ref Lookup

let speclist =
  [
    ("-i", Arg.Set ignore_case, "ignore case of needles and known entries");
    ("-v", Arg.Unit (fun () -> action := Print_Version), "print version number");
    ("--version", Arg.Unit (fun () -> action := Print_Version), "print version number");
]

let parse_arg needle = needles := needle :: !needles

let lookup () =
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

let () =
  Arg.parse speclist parse_arg "usage: goto_lookup <search>";
  match !action with
  | Print_Version -> Printf.printf "goto_lookup %s\n" Version.number
  | Lookup -> lookup ()
