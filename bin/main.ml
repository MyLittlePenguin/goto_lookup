open Goto_lookup

type action_type = Print_Version | Lookup | List

let ignore_case = ref false
let needles = ref []
let action = ref Lookup
let set_action a = (fun () -> action := a)

let speclist =
  [
    ("-i", Arg.Set ignore_case, "Ignore case of needles and known entries");
    ( "-I",
      Arg.Unit (fun () -> ignore_case := false),
      "Don't ignore case of needles and known entries" );
    ( "--list",
      Arg.Unit (set_action List),
      "Lists all matches after applying filter. If no filter was applied all \
       the known locations are listed." );
    ("-l", Arg.Unit (set_action List), "Same as --list.");
    ( "--version",
      Arg.Unit (set_action Print_Version),
      "Print version number" );
    ("-v", Arg.Unit (set_action Print_Version), "Same as --version.");
  ]

let parse_arg needle = needles := needle :: !needles

let lookup_filter () =
  filter { ignore_case = !ignore_case; needles = !needles } lines

let lookup () =
  (* !needles |> Stringify.los_to_string |> print_endline; *)
  match !needles with
  | [] -> [ "" ]
  | [ needle ] -> (
      match find { ignore_case = !ignore_case; needle } lines with
      | None -> []
      | Some p -> [ p ])
  (* | needles -> filter { ignore_case = !ignore_case; needles } lines *)
  | _ -> lookup_filter ()

let lookup_handler handler results =
  match results with [] -> exit Errors.not_found | paths -> handler paths

let () =
  Arg.parse speclist parse_arg "usage: goto_lookup <search>";
  needles := List.rev !needles;
  match !action with
  | Print_Version -> Printf.printf "goto_lookup %s\n" Version.number
  | Lookup ->
      lookup () |> lookup_handler (fun paths -> List.hd paths |> print_endline)
  | List -> lookup_filter () |> lookup_handler (List.iter print_endline)
