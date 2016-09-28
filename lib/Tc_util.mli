(** Support code for writing a type checker. *)

type id = string [@@deriving show]

type tid = string [@@deriving show]

type metavar = string [@@deriving show]

type typ =
  | TBool
  | TInt
  | TFun of typ * typ
  | TList of typ
  | TRecord of (string * typ) list
  | TArr of typ
  | TForall of tid * typ
  | TId of tid
  | TMetavar of string (** For type inference only. Ignore this in the type checker. *)
  [@@deriving show]

type op2 = Xinterp_util.op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  [@@deriving show]

type const = Xinterp_util.const =
  | Int of int
  | Bool of bool
  [@@deriving show]

type exp =
  | Id of id
  | Const of const
  | Op2 of op2 * exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * typ * exp
  | Fix of id * typ * exp
  | App of exp * exp
  | Empty of typ 
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | IsEmpty of exp
  | Record of (string * exp) list
  | GetField of exp * string
  | MkArray of exp * exp
  | GetArray of exp * exp
  | SetArray of exp * exp * exp
  | TypFun of tid * exp
  | TypApp of exp * typ
  [@@deriving show]


val from_string : string -> exp
val from_file : string -> exp
