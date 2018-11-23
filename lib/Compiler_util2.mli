(** Support code for writing a simple compiler. *)

type id = string [@@deriving show]

type op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  [@@deriving show]

type const =
  | Int of int
  | Bool of bool
  [@@deriving show]

type exp =
  | Id of id
  | Const of const
  | Op2 of op2 * exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * id list * exp
  | App of exp * exp list
  | MkArray of exp * exp
  | GetArray of exp * exp
  | SetArray of exp * exp * exp
  | Seq of exp * exp
  | Abort
  [@@deriving show]

type value =
  | Int of int
  | Bool of bool
  | Array of value array
  | Closure of (value list -> value)

module IdSet : Set.S with type elt = id

val from_string : string -> exp

val from_file : string -> exp

val show_value : value -> id

(** [eval e] evaluates [e] with an interpreter. The interpreter throws
    exceptions if [e] has type errors, free variables, etc. *)
val eval : exp -> value

(** [free_vars e] produces the free variables of [e]. *)
val free_vars : exp -> IdSet.t

(** [subst x u e] substitutes all occurrences of [x] with [u] in []e. The
 * function assumes that [u] has no free variables. *)
val subst : id -> exp -> exp -> exp