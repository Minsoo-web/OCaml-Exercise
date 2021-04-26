(* rec 키워드를 사용한 재귀함수 *)
let rec factorial a =
  if a =1 then 1 else a * factorial (a-1);;

print_int (factorial 5); print_newline();;

(* 아규먼트 두 개 *)

let multi_argu a b = 
  a + b;;

print_int (multi_argu 1 2); print_newline();;

(* 함수를 인자로 받는 경우 *)
let sum_if_true test first second =
  (if test first then first else 0) +
  (if test second then second else 0);;

let even x = x mod 2 = 0;;

print_int (sum_if_true even 4 2);;
