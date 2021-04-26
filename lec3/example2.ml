(* Functions can be also returned from a procedure *)

let plus_a a = fun b -> a+b;;

let f = plus_a 3;;
let d = plus_a 5;;

print_int(f 1) ; print_newline();;
print_int(d 1) ;;

(* Pattern Matching *)

let rec factorial a =
  match a with 1 -> 1 | _ -> a * factorial (a - 1)

(* if - else 구문 다시 쓰기 *)

let isabc c =
  if c = 'a' then true
  else if c = 'b' then true
  else if c = 'c' then true
  else false

let isabc c =
  match c with
  'a' -> true
  | 'b' -> true
  | 'c' -> true
  | _ -> false

(* or simply *)

let isabc c = 
  match c with 'a' | 'b' | 'c' -> true | _ -> false