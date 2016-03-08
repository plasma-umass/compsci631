type cmp = Lt | Gt | Eq | Lte | Gte
type aop = Add | Sub | Mul

type aexp =
  | AConst of int
  | AVar of string
  | AOp of aop * aexp * aexp

type bexp =
  | BConst of bool
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | BNot of bexp
  | BCmp of cmp * aexp * aexp

type cmd =
  | CSkip
  | CAbort
  | CAssign of string * aexp
  | CIf of bexp * cmd * cmd
  | CWhile of bexp * bexp * cmd
  | CSeq of cmd * cmd
