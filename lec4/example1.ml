let list1 = [1;2;3;];;
let list2 = [4;5;6;];;

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> hd::(append tl l2);;

append list1 list2;;

