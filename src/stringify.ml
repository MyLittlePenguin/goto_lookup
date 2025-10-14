let id x = x

let l_to_string fn list =
  let rec aux acc = function
    | [] -> acc ^ "]"
    | a :: [] -> acc ^ fn a ^ "]"
    | a :: b -> aux (acc ^ fn a ^ "; ") b
  in
  aux "[" list

let los_to_string list = l_to_string id list

