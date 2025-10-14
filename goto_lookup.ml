let home = Unix.getenv "HOME"
let got_to_file = home ^ "/.got_to"
let lines = In_channel.input_lines @@ In_channel.open_text got_to_file

let rec find_with fn = function
  | [] -> None
  | hd :: tl when fn hd -> Some hd
  | _ :: tl -> find_with fn tl

let find_perfect needle list = find_with (fun it -> it = needle) list
let find_end needle list = find_with (String.ends_with ~suffix:needle) list

let rec contains needle line =
  match line with
  | line when line = "" -> false
  | line when String.starts_with ~prefix:needle line -> true
  | line -> String.length line - 1 |> String.sub line 1 |> contains needle

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
  let write it =
    List.sort String.compare it
    |> String.concat "\n"
    |> Out_channel.(output_string @@ open_text got_to_file)
  in
  match
    ( Sys.file_exists abs_needle && Sys.is_directory abs_needle,
      List.find_index (fun it -> it = abs_needle) lines )
  with
  | true, Some x ->
      write lines;
      Some abs_needle
  | true, None ->
      write (abs_needle :: lines);
      Some abs_needle
  | false, _ -> None

let find (needle : string) (list : string list) =
  let otherwise fn = function None -> fn list | Some v -> Some v in
  let contains = contains needle in
  let find_some list = find_with contains list in
  if needle = "" then exit 400
  else
    find_dir needle
    |> otherwise (find_perfect needle)
    |> otherwise (find_end needle)
    |> otherwise find_some

let substr_after needle line =
  let length = String.length line in
  let needle_length = String.length needle in
  let rec aux length = function
    | "" -> ""
    | line when String.starts_with ~prefix:needle line ->
        String.sub line needle_length (length - needle_length)
    | line ->
        let length = length - 1 in
        aux length @@ String.sub line 1 length
  in
  aux length line

let filter needles list =
  let rec search_needles line = function
    | [] -> true
    (* | [ "" ] when line <> "" -> false *)
    | [ needle ] when contains needle line -> true
    | needle :: other_needles -> (
        match substr_after needle line with
        | "" -> false
        | remainder -> search_needles remainder other_needles)
  in
  let has_needles line = search_needles line needles in
  List.filter has_needles list
