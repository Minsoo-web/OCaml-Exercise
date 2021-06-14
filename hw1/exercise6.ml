(* HW 1 *)

open Printf
type btree = Empty | Node of int * btree * btree;;

let rec height tree = 
	match tree with 
	| Empty -> 0 
	| Node (_, left, right) -> 
		if (height left) > (height right) then (height left) + 1
		else (height right) + 1;;

(* Exercise 6 *)

let rec balanced t = 
	match t with 
	| Empty -> true
	| Node (_, l, r) -> 
	  (balanced l) && (balanced r) && ((abs ((height l) - (height r))) <= 1);;


(* Test *)

printf("%B ") (balanced Empty); print_newline();;
(* true *)

printf("%B ") (balanced (Node (2, Empty, Empty))); print_newline();;
(* true *)

printf("%B ") (balanced (Node (15, Node (3, Empty, Empty), Empty))); print_newline();;
(* true *)

printf("%B ") (balanced( Node (15, Node (3, Empty, Empty), Node (44, Empty, Empty)))); print_newline();;
(* true *)

printf("%B ") (balanced (Node (15, Node (3, Node (12, Node (7, Empty, Empty), Empty), Empty), Node (44, Node (1, Empty, Empty), Empty)))); print_newline();;
(* false *)

printf("%B ") (balanced (Node (15, Node (3, Node (12, Empty, Empty), Empty), Node (44, Node (1, Empty, Node (17, Empty, Empty)), Empty)))); print_newline();;
(* false *)

printf("%B ") (balanced (Node (15, Node (3, Node (12, Node (7, Empty, Empty), Empty), Empty), Node (44, Node (1, Empty, Node (17, Empty, Empty)), Empty)))); print_newline();;
(* false *)

printf("%B ") (balanced (Node (15, Node (3, Node (12, Empty, Empty), Node (5, Empty, Empty)), Node (44, Node (77, Empty, Empty), Node (0, Empty, Empty))))); print_newline();;
(* true *)

