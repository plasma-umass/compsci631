open Compiler_util
open Compiler_interp

let%TEST "let-binding works" =
  let prog =  from_string
      "let  x = 120 in x" in
  eval prog = VInt 120


let%TEST "factorial works" =
  let prog =  from_string
      "let factorial = fun f (n) ->
         if n == 0 then 1 else n * f(n - 1) in
       factorial(5)" in
  eval prog = VInt 120

