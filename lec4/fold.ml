(* 리스트의 모든 합을 반환 *)
let rec sum l =
  match l with
    [] -> 0
  | hd::tl -> hd + (sum tl)

(* 리스트의 모든 곱을 반환 *)
let rec prod l =
  match l with
    [] -> 0
  | hd::tl -> hd * (sum tl)


(* The code pattern can be caputred by the higher-order function fold *)
let rec fold func _list a =
  match _list with
    [] -> a
  | hd::tl -> func hd (fold func tl a)


let sum lst = fold (fun x y -> x+y) lst 0
let prod lst = fold (fun x y -> x*y) lst 1