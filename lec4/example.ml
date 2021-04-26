(* Non-tail-recursive factorial *)
let rec factorial a =
  if a =1 then 1
  else a * factorial (a-1)

(* Tail-recursive version *)
let rec fact product counter maxcounter =
  if counter > maxcounter then product
  else fact (product * counter) (counter + 1) maxcounter