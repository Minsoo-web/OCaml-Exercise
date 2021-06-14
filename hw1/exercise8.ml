(* HW 1 *)

(* Debugging function def *)

open Printf
let print_list list = 
    print_string "[ ";
    List.iter (printf "%d; ") list;
    print_string "]";;

(* Exercise 8 *)

let rec iter n input_func = 
	if n = 0 then fun x -> x 
	else 
		(fun x -> input_func ((iter (n-1) input_func) x));;


(* Test *)

print_int((iter 10 (fun x -> x + 2)) 7); print_newline();;
(* 27 *)

print_int((iter 8 (fun x -> x mod 12)) 12); print_newline();;
(* 0 *)

print_list((iter 2 (fun x -> x@x)) [12;23]); print_newline();;
(*  [ 12; 23; 12; 23; 12; 23; 12; 23; ] *)

printf("%B") ((iter 12 (fun x -> not x)) true); print_newline();;
(* true *)

print_int((iter 4 (fun x -> x * (x+1))) 2); print_newline();;
(* 3263442 *)

print_int((iter 3 (fun x -> x - x mod 2)) 7); print_newline();;
(* 6 *)

print_int((iter 8 (fun x -> x * (x-1))) 0); print_newline();;
(* 0 *)

print_int((iter 2 (fun x -> x)) 12); print_newline();;
(* 12 *)