open Xinterp_util

let%TEST "array creation" =
  from_string "array(10)" = MkArray (Const (Int 10))
                                    
let%TEST "array indexing" =
  from_string "arr[1][2]" = GetArray (GetArray (Id "arr", Const (Int 1)), Const (Int 2))

let%TEST "array update" =
  from_string "arr[1] = arr[2]" =
    SetArray (Id "arr", Const (Int 1), GetArray (Id "arr", Const (Int 2)))
