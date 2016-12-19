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

val from_string : string -> exp
val from_file : string -> exp

(** Atomic expressions. *)
type aexp =
  | AId of id
  | AConst of const
  [@@deriving show]

 (** Binding forms. *)
 and bexp =
  | BFun of id * id list * anfexp
  | BAtom of aexp
  | BOp2 of op2 * aexp * aexp
  | BMkArray of aexp * aexp
  | BGetArray of aexp * aexp
  | BSetArray of aexp * aexp * aexp
  | BApp of aexp * aexp list
  [@@deriving show]

 (** Expressions in A-normal form. *)
and anfexp =
  | ELet of id * bexp * anfexp
  | EIf of aexp * anfexp * anfexp
  | EApp of aexp * aexp list
  | ERet of aexp
  | EAbort
  [@@deriving show]

module IdSet : Set.S with type elt = id
                                     
val free_vars : anfexp -> IdSet.t

val rename : id -> id -> anfexp -> anfexp
