let isnill l = 
  match l with
    []->true 
  | _ -> false;;

print_string (string_of_bool (isnill [])); print_newline();


