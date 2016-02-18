(** Support code for writing type-inference. *)

type id = string

type op2  = Typeinf_syntax.op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul

type const = Typeinf_syntax.const =
  | Int of int (** decimal integer *)
  | Bool of bool (** either [true] or [false] *)

(** The abstract syntax of the implicitly-typed syntax and a parser. *)
module Implicit : sig

  type exp = Typeinf_syntax.Implicit.exp =
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

  val from_file : string -> exp
  val from_string : string -> exp

end

(** The abstract syntax of the explicitly-typed syntax and a pretty-printer. *)
module Explicit : sig

  type metavar = int * string

  type typ = Typeinf_syntax.Explicit.typ =
    | TMetavar of metavar
    | TInt
    | TBool
    | TFun of typ * typ
    | TPair of typ * typ
    | TList of typ

  type exp = Typeinf_syntax.Explicit.exp =
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

  (** Produces a [string] that represents the expression. *)
  val string_of_exp : exp -> string

  (** Prints the expression. *)
  val print_exp : exp -> unit


  (** Produces a [string] that represents the type. *)
  val string_of_typ : typ -> string

  (** Prints the type. *)
  val print_typ : typ -> unit

end