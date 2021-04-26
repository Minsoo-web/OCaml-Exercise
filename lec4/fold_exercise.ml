let rec length l = 
  match l with 
    [] -> 0
  | hd::tl -> 1 + length tl

let rec reverse l =
  match l with
    [] -> []
  | hd::tl -> (reverse tl)@[hd]

(* 모두 양수인지 확인하는 함수인 듯 *)
let rec is_all_pos l =
  match l with
    [] -> true
  | hd::tl -> (hd > 0) && (is_all_pos tl)


let rec fold func _list a =
  match _list with
    []->a
  | hd::tl -> func hd (fold func tl a)


(* re-write *)
let length lst = fold (fun x y ->  1 + y) lst 0
let reverse lst = fold (fun x y -> y@[x]) lst []
let is_all_pos lst = fold (fun x y -> (x >0) && y ) lst true