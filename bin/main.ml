open Goto_lookup

type action_type = Print_Version | Lookup | List

let ignore_case = ref false
let needles = ref []
let action = ref Lookup

let speclist =
  [
    ("-i", Arg.Set ignore_case, "Ignore case of needles and known entries");
    ( "-l",
      Arg.Unit (fun () -> action := List),
      "Lists all matches after applying filter. If no filter was applied all \
       the known locations are listed." );
    ("-v", Arg.Unit (fun () -> action := Print_Version), "Print version number");
    ( "--version",
      Arg.Unit (fun () -> action := Print_Version),
      "print version number" );
    ( "--list",
      Arg.Unit (fun () -> action := List),
      "Lists all matches after applying filter. If no filter was applied all \
       the known locations are listed." );
  ]

let parse_arg needle = needles := needle :: !needles

let lookup_filter () =
  filter { ignore_case = !ignore_case; needles = !needles } lines

let lookup () =
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
