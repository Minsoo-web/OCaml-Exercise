(* days 타입 *)
type days = Mon | Tue | Wed | Thu | Fri | Sat | Sun;;


(*  *)
type shape = Rect of int * int | Circle of int;;

Rect (2,3);;

let area s =
  match s with
    Rect (w,h) -> w*h
  | Circle r -> r*r*3;;

print_int (area (Rect (3,3)));;
