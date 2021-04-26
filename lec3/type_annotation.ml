let sum_if_true (test: int -> bool) (x: int) (y: int): int =
  (if test x then x else 0) + (if test y then y else 0);;

