(*  *)

let rec inc_all l =
  match l with 
    [] -> []
  | hd::tl -> (hd+1)::(inc_all tl)

(*  *)

let rec square_all l =
  match l with 
    [] -> []
  | hd::tl -> (hd*hd)::(square_all tl)

(*  *)

let rec cube_all l =
  match l with 
    [] -> []
  | hd::tl -> (hd*hd*hd)::(cube_all tl)


(*  *)

let rec map f l =
  match l with
    [] -> []
  | hd::tl -> (f hd)::(map f tl)

let inc x = x +1
let inc_all l = map inc l

let square x = x *x
let square_all l = map square l

let cube x = x *x *x
let cube_all l = map cube l

(* Or, using nameless functions *)


let inc_all l = map (fun x -> x +1) l
let square_all l = map (fun x -> x * x) l
let cube_all l = map (fun x -> x * x * x) l