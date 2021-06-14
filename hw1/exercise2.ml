(* HW 1 *)

(* Debugging function def *)

open Printf
let print_list list = 
    print_string "[ ";
    List.iter (printf "%d; ") list;
    print_string "]";;

(* Exercise 2 *)

let rec merge list1 list2 =
    match list1, list2 with
    | [], _ -> list2
    | _, [] -> list1
    | hd1::tl1, hd2::tl2 -> 
	if hd1 > hd2 then hd1 :: (merge tl1 list2)
	else hd2 :: (merge list1 tl2);;


(* Test *)

print_list (merge [3;2;1] [5;4]); print_newline();;
(* 5 4 3 2 1 *)

print_list (merge [0;0;0;0] [0;0;0;0]); print_newline();;
(* 0 0 0 0 0 0 0 *)

print_list (merge [4;3;-2] [9;7;7]); print_newline();;
(* 9 7 7 4 3 -2 *)

print_list (merge [-2;-999] []); print_newline();;
(* -2 -999 *)

print_list (merge [] [44; 1; -1023]); print_newline();;
(* 44 1 -1023 *)