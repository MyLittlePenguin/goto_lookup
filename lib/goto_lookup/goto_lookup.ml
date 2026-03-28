type query =
  | Single of { ignore_case : bool; needle : string }
  | Multi of { ignore_case : bool; needles : string list }

let index_of needle line =
  let rec aux tl =
    match tl with
    | _ when needle = "" -> Some 0
    | "" -> None
    | tl when String.starts_with ~prefix:needle tl ->
        Some (String.length line - String.length tl)
    | tl -> String.sub tl 1 (String.length tl - 1) |> aux
  in
  aux line

let substr_after needle line =
  let length = String.length line in
  let needle_length = String.length needle in
  match index_of needle line with
  | None -> ""
  | Some idx ->
      let start = idx + needle_length in
      String.sub line start (length - start)

let contains needle line =
  match index_of needle line with Some _ -> true | None -> false

let relativ_to_abs ?(path_separator = '/') path =
  let handle_relative_part acc it =
    match (acc, it) with
    | [], "." -> []
    | [], ".." -> []
    | acc, "." -> acc
    | _ :: tl, ".." -> tl
    | acc, it -> it :: acc
  in
  let separator_str = Stringify.char_to_string path_separator in
  String.split_on_char path_separator path
  |> List.fold_left handle_relative_part []
  |> List.rev
  |> String.concat separator_str

let rec find_with fn = function
  | [] -> None
  | hd :: _ when fn hd -> Some hd
  | _ :: tl -> find_with fn tl

let find_perfect prepare needle list =
  let prepared_needle = prepare needle in
  find_with (fun it -> prepare it = prepared_needle) list

let find_end prepare needle list =
  let prepared_needle = prepare needle in
  find_with
    (fun it -> String.ends_with ~suffix:prepared_needle (prepare it))
    list

let find_dir ?(cwd = Sys.getcwd ()) ?(path_separator = '/') needle =
  let path_separator_str = Stringify.char_to_string path_separator in
  let parent =
    relativ_to_abs ~path_separator @@ cwd ^ path_separator_str ^ ".."
  in
  let full_path = function
    | ".." -> parent
    | "." -> cwd
    | x when String.starts_with ~prefix:(".." ^ path_separator_str) x ->
        parent ^ String.sub x 2 (String.length x - 2)
    | x when String.starts_with ~prefix:("." ^ path_separator_str) x ->
        cwd ^ String.sub x 1 (String.length x - 1)
    | x when Filename.is_relative x -> cwd ^ path_separator_str ^ x
    | x -> x
  in
  let abs_needle = needle |> full_path |> relativ_to_abs in
  let abs_needle =
    if String.ends_with ~suffix:path_separator_str abs_needle then
      String.sub abs_needle 0 (String.length abs_needle - 1)
    else abs_needle
  in
  if Sys.file_exists abs_needle && Sys.is_directory abs_needle then
    Some abs_needle
  else None

let get_preparator = function
  | false -> fun str -> str
  | true -> fun str -> String.lowercase_ascii str

let filter (query : query) list =
  match query with
  | Single { ignore_case; needle } -> (
      let prep = get_preparator ignore_case in
      let needle = prep needle in
      match needle with
      | "" -> list
      | needle -> List.filter (fun it -> prep it |> contains needle) list)
  | Multi { ignore_case; needles } ->
      let prep = get_preparator ignore_case in
      let needles = List.map prep needles in
      let rec search_needles line = function
        | [] -> true
        | [ needle ] when contains needle line -> true
        | needle :: other_needles -> (
            match substr_after needle line with
            | "" -> false
            | remainder -> search_needles remainder other_needles)
      in
      let has_needles line = search_needles (prep line) needles in
      List.filter has_needles list

let otherwise fn list = function None -> fn list | Some v -> Some v

let rec find (query : query) (list : string list) =
  match query with
  | Single { ignore_case; needle } ->
      let prepare = get_preparator ignore_case in
      let prepared_needle = prepare needle in
      let contains line = contains prepared_needle (prepare line) in
      let find_some list = find_with contains list in
      None
      |> otherwise (find_perfect prepare needle) list
      |> otherwise (find_end prepare needle) list
      |> otherwise find_some list
  | Multi { ignore_case; needles } as origin ->
      find
        (Single
           { ignore_case; needle = List.fold_left (fun _ it -> it) "" needles })
        (filter origin list)
