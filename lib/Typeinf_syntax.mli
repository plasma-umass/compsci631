type id = string

type op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul

type const =
  | Int of int (** decimal integer *)
  | Bool of bool (** either [true] or [false] *)

module Implicit : sig

  type exp =
    | Const of const
    | Op2 of op2 * exp * exp
    | If of exp * exp * exp
    | Id of id
    | Let of id * exp * exp
    | Fun of id * exp
    | Fix of id * exp
    | App of exp * exp
    | Empty
    | Cons of exp * exp
    | Head of exp
    | Tail of exp
    | IsEmpty of exp
    | Pair of exp * exp
    | ProjL of exp
    | ProjR of exp

end

module Explicit : sig

  type metavar = int * string

  type typ =
    | TMetavar of metavar
    | TInt
    | TBool
    | TFun of typ * typ
    | TPair of typ * typ
    | TList of typ

  type exp =
    | Const of const
    | Op2 of op2 * exp * exp
    | If of exp * exp * exp
    | Id of id
    | Let of id * exp * exp
    | Fun of id * typ * exp
    | Fix of id * typ * exp
    | App of exp * exp
    | Empty of typ
    | Cons of exp * exp
    | Head of exp
    | Tail of exp
    | IsEmpty of exp
    | Pair of exp * exp
    | ProjL of exp
    | ProjR of exp

end