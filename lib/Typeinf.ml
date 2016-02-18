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

module Implicit = struct

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

  module Parsers = Lexparse_util.MakeParsers (struct
      exception ParseError = Typeinf_parser.Error
      type token = Typeinf_parser.token
      type exp = Typeinf_syntax.Implicit.exp
      let parser = Typeinf_parser.program
      let lexer = Typeinf_lexer.token
    end)

  let from_file = Parsers.from_file
  let from_string = Parsers.from_string

end

module Explicit = struct

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

include Typeinf_pretty

end