let home = Sys.getenv "HOME"
let got_to_file = home ^ "/.got_to"
let lines = In_channel.input_lines @@ In_channel.open_text got_to_file

type query =
  | Single of { ignore_case : bool; needle : string }
  | Multi of { ignore_case : bool; needles : string list }

type multi_needle_query = { ignore_case : bool; needles : string list }
type single_needle_query = { ignore_case : bool; needle : string }

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

let find_dir needle =
  let cwd = Sys.getcwd () in
  let parent =
    (* find last path element and substract its length (+ 1 for /) from the total path *)
    String.fold_left
      (fun acc it -> if it = '/' then "" else acc ^ String.make 1 it)
      "" cwd
    |> String.length |> ( + ) 1
    |> ( - ) (String.length cwd)
    |> String.sub cwd 0
  in
  let to_abs = function
    | ".." -> parent
    | "." -> cwd
    | x when String.starts_with ~prefix:"../" x ->
        parent ^ String.sub x 2 @@ (String.length x - 2)
    | x when String.starts_with ~prefix:"./" x ->
        cwd ^ String.sub x 1 @@ (String.length x - 1)
    | x when Filename.is_relative x -> cwd ^ "/" ^ x
    | x -> x
  in
  let abs_needle = to_abs needle in
  let abs_needle =
    if String.ends_with ~suffix:"/" abs_needle then
      String.sub abs_needle 0 (String.length abs_needle - 1)
    else abs_needle
  in
  let write it =
    List.sort String.compare it
    |> String.concat "\n"
    |> Out_channel.(output_string @@ open_text got_to_file)
  in
  match
    ( Sys.file_exists abs_needle && Sys.is_directory abs_needle,
      List.find_index (fun it -> it = abs_needle) lines )
  with
  | true, Some _ ->
      write lines;
      Some abs_needle
  | true, None ->
      write (abs_needle :: lines);
      Some abs_needle
  | false, _ -> None

let get_preparator = function
  | false -> fun str -> str
  | true -> fun str -> String.lowercase_ascii str

let find (query : single_needle_query) (list : string list) =
  let prepare = get_preparator query.ignore_case in
  let otherwise fn = function None -> fn list | Some v -> Some v in
  let prepared_needle = prepare query.needle in
  let contains line = contains prepared_needle (prepare line) in
  let find_some list = find_with contains list in
  if query.needle = "" then exit 400
  else
    find_dir query.needle
    |> otherwise (find_perfect prepare query.needle)
    |> otherwise (find_end prepare query.needle)
    |> otherwise find_some

let filter (query : multi_needle_query) list =
  let prep = get_preparator query.ignore_case in
  let needles = List.map prep query.needles in
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
