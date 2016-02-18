exception Error of string

module type PARSER = sig
  exception ParseError
  type token
  type exp

  val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> exp
  val lexer : Lexing.lexbuf -> token
end

module MakeParsers : functor (Parser : PARSER) -> sig
  val from_file : string -> Parser.exp
  val from_string : string -> Parser.exp
end