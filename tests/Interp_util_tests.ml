open Interp_util

let%TEST "normal subtraction associaticity" =
  from_string "1 - 2 - 3" =
   Op2 (Sub, Op2 (Sub, Const (Int 1), Const (Int 2)), Const (Int 3))

let%TEST "a list of three integers" =
  from_string "1 :: 2 :: 3 :: empty" =
    Cons(Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Empty)))
