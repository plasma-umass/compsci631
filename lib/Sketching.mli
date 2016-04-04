type op2 =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BAnd
  | BOr
  | LAnd
  | LOr
  | Eq

type op1 = LNot | BNot

type id = string

type exp =
  | EId of id
  | EInt of int
  | EWidth
  | EOp2 of op2 * exp * exp
  | EOp1 of op1 * exp
  | EHole of int

type cmd =
  | CSkip
  | CAbort
  | CAssign of id * exp
  | CIf of exp * cmd * cmd
  | CSeq of cmd * cmd
  | CRepeat of id * exp * cmd

val from_string : string -> cmd
val from_file : string -> cmd
val to_string : cmd -> string