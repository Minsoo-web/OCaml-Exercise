let compose f g = fun x -> f(g(x))

let empty_map = fun x -> raise (Failure "not exist!");;

let add_map (k,v) map =
  fun x -> if (k=x) then v else (map x);;

let m = (compose (add_map (1, "one")) (add_map (2, "two"))) empty_map;;