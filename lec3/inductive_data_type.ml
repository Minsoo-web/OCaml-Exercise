type mylist = Nil | List of int * mylist;;

Nil;;

let rec mylength l =
  match l with
    Nil -> 0
  | List(_, l') ->1 + mylength l';;

print_int (mylength (List(1, List (1, Nil))));;