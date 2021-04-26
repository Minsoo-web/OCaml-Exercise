let rec insert number _list =
  match _list with
    [] -> [number]
  | hd::tl -> if (hd >= number) then [number]@_list else [hd]@(insert number tl)

(* sort *)

let rec sort _list =
  match _list with
    [] -> []
  | hd::tl -> insert hd (sort tl)
