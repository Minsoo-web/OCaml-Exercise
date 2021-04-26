
let rec remove_first number _list=
  match _list with
    [] -> []
  | hd::tl -> if(hd = number) then tl else hd::(remove_first number tl);;