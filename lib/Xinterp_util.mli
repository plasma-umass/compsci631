(** Support code for writing an interpreter for a functional-imperative language. *)

type id = string [@@deriving show]

type op2 = Interp_util.op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  [@@deriving show]

type const = Interp_util.const =
  | Int of int
  | Bool of bool
  [@@deriving show]

type exp =
  | Id of id
  | Const of const
  | Op2 of op2 * exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * exp
  | Fix of id * exp
  | App of exp * exp
  | Empty
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | IsEmpty of exp
  | Record of (string * exp) list
  | GetField of exp * string
  | MkArray of exp * exp
  | GetArray of exp * exp
  | SetArray of exp * exp * exp
  [@@deriving show]

val from_string : string -> exp
val from_file : string -> exp
