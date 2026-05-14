open Lookup

let home = Sys.getenv "HOME"
let got_to_file = home ^ "/.got_to"
let lines = In_channel.input_lines @@ In_channel.open_text got_to_file

type action_type =
  | Print_Version
  | Print_Help
  | Lookup
  | List
  | Clean
  | Remove_List
  | Remove_Single
  | Print_Orphaned

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
      (fun (action, ignore_case, needles) ->
        match action with
        | Remove_Single | Remove_List -> (Remove_List, ignore_case, needles)
        | _ -> (List, ignore_case, needles)),
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
    ( "",
      "--show-orphaned",
      (fun (_, ignore_case, needles) -> (Print_Orphaned, ignore_case, needles)),
      "show orphaned entries from the list of known locations" );
    ( "-d",
      "--delete",
      (fun (action, ignore_case, needles) ->
        match action with
        | List -> (Remove_List, ignore_case, needles)
        | _ -> (Remove_Single, ignore_case, needles)),
      "delete entries found by the query" );
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
  print_endline "Usage: lookup [options] [query]";
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

let print_orphaned () =
  lines
  |> List.filter (fun it -> not (Sys.file_exists it && Sys.is_directory it))
  |> List.iter print_endline

let remove_paths paths =
  lines
  |> List.filter (fun it -> List.exists (( = ) it) paths |> not)
  |> String.concat "\n"
  |> Out_channel.(output_string @@ open_text got_to_file)

let add_dir list path =
  path :: list |> List.sort String.compare |> String.concat "\n"
  |> Out_channel.(output_string @@ open_text got_to_file)

let add_dir_if_neccessary = function
  | Some dir -> (
      match List.find_index (( = ) dir) lines with
      | Some _ -> Some dir
      | None ->
          add_dir lines dir;
          Some dir)
  | None -> None

let () =
  let action, query = parse_args Sys.argv in
  match action with
  | Print_Version -> Printf.printf "lookup %s\n" Version.number
  | Print_Help -> print_help ()
  | Lookup -> (
      let result =
        match query with
        | Single { needle = ""; _ } -> Some ""
        | Single { needle; _ } ->
            find_dir needle |> add_dir_if_neccessary
            |> otherwise (find query) lines
        | _ -> find query lines
      in
      match result with
      | None -> exit Errors.not_found
      | Some line -> print_endline line)
  | List -> filter query lines |> List.iter print_endline
  | Clean -> remove_dead_paths ()
  | Print_Orphaned -> print_orphaned ()
  | Remove_Single -> (
      match query with
      | Single { needle = ""; _ } ->
          print_endline "Query must not be empty for deletion"
      | _ -> (
          find query lines |> Option.map (fun it -> [ it ]) |> function
          | Some it -> remove_paths it
          | None -> ()))
  | Remove_List -> (
      match query with
      | Single { needle = ""; _ } ->
          print_endline "Query must not be empty for deletion"
      | _ -> filter query lines |> remove_paths)
