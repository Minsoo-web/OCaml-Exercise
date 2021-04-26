let div a b = 
  try
    a / b
  with Division_by_zero -> 0;;

let result =  div 10 0;;

print_int result;;

(* user-defined exception *)

exception Problem;;

let mydiv a b =
  if b = 0 then raise Problem
  else a / b ;;

mydiv 1 0;;

(* or *)

let result2 = try 
    mydiv 10 0
  with Problem -> 0;;

print_int result2;;