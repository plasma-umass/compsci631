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

type typ =
  | TInt
  | TBool
  | TFun of typ * typ
  | TRecord of (string * typ)
  | TList of typ
  [@@deriving show]

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
  | Record of (string * exp) list
  | GetField of exp * string
  [@@deriving show]


