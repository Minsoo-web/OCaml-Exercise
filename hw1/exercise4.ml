(* HW 1 *)

open Printf

(* type def *)

type formula = TRUE | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
  and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec calc exp = 
	match exp with 
	| PLUS (exp1, exp2) -> (calc exp1) + (calc exp2)
	| MINUS (exp1, exp2) -> (calc exp1) - (calc exp2)
	| NUM n -> n;;

(* Exercise 4 *)

let rec eval form =
	match form with 
	| TRUE -> true 
	| FALSE -> false
	| NOT form' -> not (eval form')
	| ANDALSO (form1, form2) -> (eval form1) && (eval form2) 
	| ORELSE (form1, form2) -> (eval form1) || (eval form2)
	| IMPLY (form1, form2) -> (not (eval form1)) || (eval form2)  
	| LESS (expr1, expr2) -> (calc expr1) < (calc expr2);;

printf("%B") (eval (IMPLY(FALSE, TRUE))); print_newline();;
(* true *)

printf("%B") (eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE))))); print_newline();;
(* true *)

printf("%B") ((eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE))))); print_newline();;
(* true *)

printf("%B") ((eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE))))); print_newline();;
(* true *)

printf("%B") ((eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE)))); print_newline();;
(* true *)

printf("%B") (eval (LESS (NUM 5, PLUS(NUM 1, NUM 6)))); print_newline();;
(* true *)

printf("%B") (eval (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2)))); print_newline();;
(* false *)

printf("%B") (eval (LESS (MINUS (NUM 10, NUM 1), MINUS (NUM 1, NUM 2)))); print_newline();;
(* false *)

printf("%B") (eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE))); print_newline();;
(* false *)

printf("%B") (eval (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1)))))); print_newline();;
(* true *)