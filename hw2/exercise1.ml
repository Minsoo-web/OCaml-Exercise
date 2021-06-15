(* HW 2 *)

open Printf

(* Exercise 1 *)

type exp = X | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp;;

let rec sigma l u e =
  if l > u then 0.0 else (calc_with_x_value e l) +. (sigma (l +. 1.0) u e)

and integral l u e =
  if l > u then 0.0 else (0.1 *. (calc_with_x_value e l)) +. (integral (l +. 0.1) u e)

and calc_with_x_value e v =
  match e with
  | X -> v
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calc_with_x_value e1 v) +. (calc_with_x_value e2 v)
  | SUB (e1, e2) -> (calc_with_x_value e1 v) -. (calc_with_x_value e2 v)
  | MUL (e1, e2) -> (calc_with_x_value e1 v) *. (calc_with_x_value e2 v)
  | DIV (e1, e2) -> (calc_with_x_value e1 v) /. (calc_with_x_value e2 v)
  | SIGMA (e1, e2, e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    sigma l u e3
  | INTEGRAL (e1, e2 ,e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    integral l (u -. 0.1) e3;;

let rec calculate e =
  match e with
  | X -> raise (Failure "FreeVariable")
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calculate e1) +. (calculate e2)
  | SUB (e1, e2) -> (calculate e1) -. (calculate e2)
  | MUL (e1, e2) -> (calculate e1) *. (calculate e2)
  | DIV (e1, e2) -> (calculate e1) /. (calculate e2)
  | SIGMA (e1, e2, e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    sigma l u e3
  | INTEGRAL (e1, e2 ,e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    integral l (u -. 0.1) e3;;


(* Test Header *)

let equals v1 v2 = (abs_float (v1 -. v2)) <= 1.0;;

let test t answer =
  let v = calculate t in
  printf("%B ") (equals v answer); print_newline();;

(* Test Body *)

let t1 : exp = ADD(INT 2, REAL 3.5) in
test t1 5.5;;
(* true *)

let t2 : exp = DIV(REAL 3.0, INT 2) in
test t2 1.5;;
(* true *)

let t31 : exp = ADD(INT 2, REAL 3.5) in
let t32 : exp = MUL(INT 2, REAL 5.5) in
let t33 : exp = DIV(t32, t31) in
test t33 2.0;;
(* true *)

let t4 : exp = SIGMA (INT 1, INT 10, X) in
test t4 55.0;;
(* true *)

let t5  : exp = INTEGRAL(INT 0, INT 10, INT 20) in
test t5 200.;;
(* true *)

let t6  : exp = SIGMA (INT 1, INT 5, SIGMA (X, X, SIGMA (X, ADD (X, INT 1), X))) in test t6 35.0;;
(* true *)

let t7  : exp = SIGMA (INT 1, INT 3, SIGMA (INT 1, INT 5, X)) in
test t7 45.0;;
(* true *)

let t8  : exp = SIGMA (INT 1, INT 10, ADD(MUL(X, X), INT 10)) in
test t8 (485.0);;
(* true *)


let t9 = SIGMA(INT 1, INT 10, (SIGMA (X, MUL(X,X), X))) in
test t9 12694.;;
(* true *)

calculate (SIGMA (INT 1, X, INTEGRAL (X, X, SIGMA (X, ADD (X, INT 1), X))));;
(* FreeVariable *)