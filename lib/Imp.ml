include Imp_syntax

module Parsers = Lexparse_util.MakeParsers (struct
    exception ParseError = Parsing.Parse_error
    type token = Imp_parser.token
    type exp = cmd
    let parser = Imp_parser.program
    let lexer = Imp_lexer.token
  end)

let from_file = Parsers.from_file
let from_string = Parsers.from_string