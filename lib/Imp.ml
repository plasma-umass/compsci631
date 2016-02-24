type aexp = Imp_syntax.aexp =
  | AConst of int
  | AVar of string
  | AAdd of aexp * aexp
  | AMul of aexp * aexp

type bexp = Imp_syntax.bexp =
  | BConst of bool
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | BNot of bexp
  | BLT of aexp * aexp

type cmd = Imp_syntax.cmd =
  | CSkip
  | CAbort
  | CAssign of string * aexp
  | CIf of bexp * cmd * cmd
  | CWhile of bexp * bexp * cmd
  | CSeq of cmd * cmd


module Parsers = Lexparse_util.MakeParsers (struct
    exception ParseError = Parsing.Parse_error
    type token = Imp_parser.token
    type exp = cmd
    let parser = Imp_parser.program
    let lexer = Imp_lexer.token
  end)

let from_file = Parsers.from_file
let from_string = Parsers.from_string