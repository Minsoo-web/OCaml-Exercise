let rec reverse l1 = 
  match l1 with
    [] -> []
  |hd::tl -> (reverse tl)@[hd];;
