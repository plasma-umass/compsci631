open Tc_util

let%TEST "Empty takes a type parameter" =
  from_string "empty<int>" =  Empty TInt

let%TEST "Arrow type associativity" =
  from_string "let f = fun (x : int -> bool -> int) -> 1 in 0" =
    Let ("f", Fun ("x", TFun (TInt, TFun (TBool, TInt)), Const (Int 1)), Const (Int 0))

let%TEST "Nested list type" =
  from_string "empty<int list list>" =
    Empty (TList (TList TInt))

let%TEST "type application" =
  from_string "f<int -> int> 10" =
    App (TypApp (Id "f", TFun (TInt, TInt)), Const (Int 10))

let%TEST "A variable name can start with a keyword" =
  from_string "let funx = 700 in funx" =
    Let ("funx", Const (Int 700), Id "funx")

let%TEST "Type functions parse expression" =
  from_string "tfun alpha.fun (x : alpha) -> x" =
    TypFun ("alpha", Fun ("x", TId "alpha", Id "x"))

let%TEST "Fix parses correctly" =
  from_string "fix (f : int) -> f" =
    Fix ("f", TInt, Id "f")
              
