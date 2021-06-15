(* HW 2 *)

open Printf;;

type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list;;

(* Prob. 2 *)

(* 미분 *)
let rec diff (ae, s) =
  match ae with
  | CONST i -> CONST 0 (* 상수는 0 *)
  | VAR v -> if v = s then CONST 1 else CONST 0
  | POWER (v, i) -> 
		if v = s then TIMES [CONST i; POWER (v, i-1)] else CONST 0
  | TIMES aes ->
		SUM (List.map (fun ae -> 
          let rec remains aei res = 
            match aei with
            | h::t -> if (Stdlib.compare ae h) == 0 then res@t else remains t (res@[h])
            | [] -> res
          in
					let remaining = remains aes [] in   
					TIMES (diff (ae, s)::remaining)
				) aes) 
  | SUM aes -> 
		SUM (List.map (fun ae -> diff (ae, s)) aes);;

(* Test Header *)

let (|>) g f = f g;;

type r = (int * ((string * int) list)) list;;

let rec sum r1 r2 = 
    match r1, r2 with
    | _, [] -> r1
    | [], _ -> r2
    | (c1, xs1)::t1, (c2, xs2)::t2 ->
        if xs1 = xs2 then (c1 + c2, xs1)::(sum t1 t2)
        else if xs1 < xs2 then 
            (c1, xs1)::(sum t1 r2)
        else
            (c2, xs2)::(sum r1 t2);;

let rec mult res a =

    match a with
    | CONST c1 -> 
        List.map (fun (c, xs) -> (c1 * c, xs)) res
	
    | VAR v ->
        mult res (POWER (v, 1))

    | POWER (x1, n1) ->
        let r = 
            List.map (fun (c, xs) ->
                let rec iter rlst = 
                    match rlst with
                    | [] -> [(x1, n1)]
                    | (x2, n2)::tl ->
                        if x1 = x2 then (x2, n1 + n2)::tl
                        else if x1 < x2 then (x1, n1)::rlst
                        else (x2, n2)::(iter tl) in
                (c, iter xs)) res in
        List.fold_left (fun res elem -> sum res [elem]) [] r  

    | SUM alst -> (
        match alst with
        | [] -> []
        | a::tl -> sum (mult res a) (mult res (SUM tl))
    )

    | TIMES alst ->
    (
        match alst with
        | [] -> res
        | a::tl -> mult (mult res a) (TIMES tl)
    );;


let normalize a = 
    (mult [1, []] a)
    |> List.filter (fun (c, _) -> c <> 0);;


let equals n1 a = 
    n1 = (normalize a);;


(* Test Body *)

printf("%B ") (diff (CONST 1, "x") |> equals []); print_newline();;
(* true *)

printf("%B ") (diff (VAR "y", "x") |> equals []); print_newline();;
(* true *)

printf("%B ") (diff (VAR "x", "x") |> equals [(1, [])]); print_newline();;
(* true *)

printf("%B ") (diff (POWER ("xxx",4) , "xxx") |> equals [(4, [("xxx", 3)])]); print_newline();;
(* true *)

printf("%B ") (diff (POWER ("xy",3) , "yx") |> equals []); print_newline();;
(* true *)

printf("%B ") (diff (TIMES [CONST 3; VAR "x" ; POWER ("y", 2) ; VAR "z"] , "y") |> equals [6, ["x", 1; "y", 1; "z", 1]]); print_newline();;
(* true *)

printf("%B ") (diff (SUM [(TIMES [CONST 3; VAR "x" ; POWER ("y",2) ; VAR "z"]); VAR "y"] , "x") |> equals [3, ["y", 2; "z", 1]]); print_newline();;
(* true *)

printf("%B ") (diff (SUM [(TIMES [CONST 2; POWER ("x",2)]); (TIMES [CONST 3; VAR "x"]); CONST 4] , "x") |> equals [3, []; 4, ["x", 1]]); print_newline();;
(* true *)

printf("%B ") (diff(SUM[TIMES[CONST 10;VAR "x"];VAR "y"], "xy") |> equals []); print_newline();;
(* true *)

printf("%B ") (diff(TIMES[VAR "x"; VAR "x"], "x") |> equals [(2, [("x", 1)])]); print_newline();;
(* true *)
