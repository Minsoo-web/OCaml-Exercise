(* HW 1 *)

(* Exercise 7 *)

let rec fold3 input_func number blst clst dlst = 
	match blst, clst, dlst with 
	| hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 -> 
		fold3 input_func (input_func number hd1 hd2 hd3) tl1 tl2 tl3;
	| [], [], [] -> number 
	| _ -> raise (Failure "fail");;

(* Test *)

print_int(fold3 (fun a b c d -> a + b + c + d) 10 [33;67;12;33] [10;23;84;57] [11;55;23;58]); print_newline();;
(* 476 *)

print_int(fold3 (fun a b c d -> (-a) + b + c + d) 4 [11;63;-45;22] [75;123;-44;1] [55;24;20;3]); print_newline();;
(* 168 *)

print_int(fold3 (fun a b c d -> a + b * c + d) 12 [24;67;1;77;23;54] [11;3;8;0;22;13] [123;55;37;89;2;45]); print_newline();;
(* 2044 *)

print_int(fold3 (fun a b c d -> a * b * c * d) 55 [] [] []); print_newline();;
(* 55 *)

print_int(fold3 (fun a b c d -> (a * b * c + d) mod 7) 33 [12;33] [10;7] [5;12]); print_newline();;
(* 5 *)

print_int(fold3 (fun a b c d -> (a mod 3) + b + c + d) 5 [12;567;33;0] [123;44;6;33] [116;76;223;8]); print_newline();;
(* 43 *)

print_int(fold3 (fun a b c d -> if b then a + c else a + d) 34 [true;false;false;true] [12;3;4;77] [11;23;6;100]); print_newline();;
(* 152 *)

print_int(fold3 (fun a b c d -> if b then a else c + d) 55 [true;true;false;false;true] [111;63;88;123;98] [0;23;778;34;6]); print_newline();;
(* 157 *)
