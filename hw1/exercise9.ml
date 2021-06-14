(* HW 1 *)

(* Exercise 9 *)

let rec sigma (a,b,input_func) = 
	if a > b then 0 
	else 
		(input_func a) + sigma (a+1, b, input_func);;

(* Test *)

print_int(sigma (10, 10, (fun x -> x))); print_newline();;
(* 10 *)

print_int(sigma (11,10,(fun x -> x))); print_newline();;
(* 0 *)

print_int(sigma (10,5,(fun x -> x))); print_newline();;
(* 0 *)

print_int(sigma (1,10,(fun x -> if x mod 2 = 0 then 1 else 0))); print_newline();;
(* 5 *)

print_int(sigma (1,10,(fun x -> x * x))); print_newline();;
(* 385 *)

print_int(sigma (10,12,(fun x -> 2 * x))); print_newline();;
(* 66 *)

print_int(sigma (0,10,(fun x -> if x -1 > 0 then x-1 else 0))); print_newline();;
(* 45 *)

print_int(sigma (0,100,(fun x -> 0))); print_newline();;
(* 0 *)

print_int(sigma (2,10,(fun x -> x + 10))); print_newline();;
(* 144 *)
