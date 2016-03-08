{
  open Lexing
  open Smtlib_parser
}

let simple_symbol_char = [ 'A'-'Z' 'a'-'z' '+' '-' '/' '*' '=' '%' '?' '!' '.' '$' '_' '~' '&' '^' '<' '>' '@']
let simple_symbol = simple_symbol_char (['0' - '9'] | simple_symbol_char )*
let keyword = ':' simple_symbol
let blank = [ ' ' '\t' ]
let numeral = ((['1'-'9']['0'-'9']*) | ['0'])

rule token = parse
  | ";" [^ '\n' '\r']+ { token lexbuf }
  | "\r\n" { new_line lexbuf; token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | blank+ { token lexbuf }
  | eof { EOF }
  | numeral as n { INT (int_of_string n) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | simple_symbol as x { SYMBOL x }
  | keyword as x { KEYWORD x }

and block_comment = parse
  | "*/" { token lexbuf }
  | "*" { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | "\n" { new_line lexbuf; block_comment lexbuf }
  | ([^ '*' '\n'])+  { block_comment lexbuf }
