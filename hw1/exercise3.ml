(* HW 1 *)

(* Debugging function def *)

open Printf
let print_list list = 
    print_string "[ ";
    List.iter (printf "%d; ") list;
    print_string "]";;


(* Exercise 3 *)

let rec range lower upper = 
	if (lower > upper) then [] 
	else lower :: (range (lower + 1) upper);;

(* Test *)

print_list (range 10 15); print_newline();;
(* [ 10; 11; 12; 13; 14; 15; ] *)

print_list (range (-2) 7); print_newline();;
(* [ -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; ] *)

print_list (range 9 3); print_newline();;
(* [ ] *)

print_list (range 22 22); print_newline();;
(* [22;] *)

print_list (range 55 (-12)); print_newline();;
(* [ ] *)
