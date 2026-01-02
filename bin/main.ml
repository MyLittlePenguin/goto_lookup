open Goto_lookup

type action_type = Print_Version | Print_Help | Lookup | List | Clean

let specs =
  [
    ( "-i",
      "--ignore-case",
      (fun (action, _, needles) -> (action, true, needles)),
      "ignore case for finding the matches in the known locations" );
    ( "-I",
      "--no-ignore-case",
      (fun (action, _, needles) -> (action, false, needles)),
      "do not ignore case for finding the matches in the known locations" );
    ( "-l",
      "--list",
      (fun (_, ignore_case, needles) -> (List, ignore_case, needles)),
      "list all matching locations" );
    ( "-v",
      "--version",
      (fun (_, ignore_case, needles) -> (Print_Version, ignore_case, needles)),
      "print version information" );
    ( "-h",
      "--help",
      (fun (_, ignore_case, needles) -> (Print_Help, ignore_case, needles)),
      "print help information" );
    ( "",
      "--clean",
      (fun (_, ignore_case, needles) -> (Clean, ignore_case, needles)),
      "remove orphaned entries from the list of known locations" );
  ]

let parse_args args =
  let state = (Lookup, false, []) in
  let len = Array.length args in
  let args = Array.sub args 1 (len - 1) in
  let parsed_args =
    Array.fold_left
      (fun acc it ->
        match
          List.find_opt
            (fun (short, long, _, _) ->
              let short = String.trim short in
              let long = String.trim long in
              (short <> "" && short = it) || (long <> "" && long = it))
            specs
        with
        | Some (_, _, action, _) -> action acc
        | None ->
            let action, ignore_case, needles = acc in
            (action, ignore_case, it :: needles))
      state args
  in
  let action, ignore_case, needles = parsed_args in
  match needles with
  | [] -> (action, Single { ignore_case; needle = "" })
  | [ a ] -> (action, Single { ignore_case; needle = a })
  | _ -> (action, Multi { ignore_case; needles = List.rev needles })

let print_help () =
  print_endline "Usage: goto_lookup [options] [query]";
  print_endline "";
  print_endline "Options";
  List.iter
    (fun (short, long, _, desc) ->
      if "" = String.trim short then
        Printf.printf "        %-20s %s\n" long desc
      else Printf.printf "    %s, %-20s %s\n" short long desc)
    specs

let remove_dead_paths () =
  lines
  |> List.filter Sys.file_exists
  |> List.filter Sys.is_directory
  |> String.concat "\n"
  |> Out_channel.(output_string @@ open_text got_to_file)

let () =
  let action, query = parse_args Sys.argv in
  match action with
  | Print_Version -> Printf.printf "goto_lookup %s\n" Version.number
  | Print_Help -> print_help ()
  | Lookup -> (
    let result = match query with
      | Single { ignore_case = _; needle } ->
          find_dir needle |> otherwise (find query) lines
      | _ -> find query lines
    in
    match result with
      | None -> exit Errors.not_found
      | Some line -> print_endline line
      )
  | List -> filter query lines |> List.iter print_endline
  | Clean -> remove_dead_paths ()
