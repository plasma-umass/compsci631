open Interp_util

let%TEST "normal subtraction associaticity" =
  from_string "1 - 2 - 3" =
   Op2 (Sub, Op2 (Sub, Const (Int 1), Const (Int 2)), Const (Int 3))

let%TEST "a list of three integers" =
  from_string "1 :: 2 :: 3 :: empty" =
    Cons(Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Empty)))

let%TEST "a list of three integers" =
  from_string "1 :: 2 :: 3 :: empty" =
    Cons(Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Empty)))

let%TEST "Conrad\'s example" =
  from_string "let f = (fun x -> 1) in 1" =
    Let ("f", Fun ("x", Const (Int 1)), Const (Int 1))

let%TEST "Harsh example 1" =
  from_string "if true then 700 else 600" =
    If (Const (Bool true), Const (Int 700), Const (Int 600))

let%TEST "Harsh example 2" =
  from_string "let x = 700 in x" =
    Let ("x", Const (Int 700), Id "x")

let%TEST "chained application" =
  from_string "f x y" =
    App (App (Id "f", Id "x"), Id "y")

let%TEST "A variable name can start with a keyword" =
  from_string "let funx = 700 in funx" =
    Let ("funx", Const (Int 700), Id "funx")

