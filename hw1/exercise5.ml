(* HW 1 *)

(* Exercise 5 *)

type btree = Empty | Node of int * btree * btree

let rec height tree = 
	match tree with 
	| Empty -> 0 
	| Node (_, left, right) -> 
		if (height left) > (height right) then (height left) + 1
		else (height right) + 1;;


(* Test *)

print_int (height Empty); print_newline();;
(* 0 *)

print_int (height (Node (10, Node (3, Empty, Node (12, Empty, Empty)), Node (123, Node (15, Empty, Empty), Empty)))); print_newline();;
(* 3 *)

print_int (height (Node (17, Node (22, Node (45, Node (14, Empty, Empty), Empty), Empty), Empty))); print_newline();;
(* 4 *)

print_int (height (Node (115, Node (14, Empty, Empty), Node (33, Empty, Node (17, Empty, Node (23, Empty, Empty)))))); print_newline();;
(* 4 *)

print_int (height (Node (15, Node (-2, Empty, Empty), Node (11, Node (0, Empty, Empty), Empty)))); print_newline();;
(* 3 *)

print_int (height (Node (111, Node (123, Node (33, Node (-5, Node (12, Empty, Empty), Empty), Empty), Node (-125, Empty, Empty)), Node (22, Empty, Node (99, Empty, Empty))))); print_newline();;
(* 5 *)

print_int (height (Node (11, Empty, Node (15, Node (12, Empty, Empty), Node (55, Node (23, Empty, Empty), Empty))))); print_newline();;
(* 4 *)
