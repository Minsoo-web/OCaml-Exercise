(* HW 1 *)

(* Exercise 1 *)

let rec gcd n m = 
    let n,m = if (n >= m) then (n,m) else (m, n) in
    if (m = 0) then n
    else gcd (n - m) m;;

(* Test *)

print_int (gcd 10 0); print_newline();;
(* 10 *)

print_int (gcd 9 5); print_newline();; 
(* 1 *)

print_int (gcd 13 13); print_newline();;
(* 13 *)

print_int (gcd 37 600); print_newline();; 
(* 1 *)

print_int (gcd 20 100); print_newline();; 
(* 20 *)

print_int (gcd 624129 2061517); print_newline();; 
(* 18913 *)

print_int (gcd 0 0); print_newline();; 
(* 0 *)