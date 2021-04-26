let rec nth l n =
  match l with
  | [] -> raise (Failure "list is too short")
  | hd::tl ->  if (n=0) then hd else nth tl (n-1);;