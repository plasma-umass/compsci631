{
  open Lexing
  open Imp_parser
}

let blank = [ ' ' '\t' ]
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let decimal = '-'? ((['1'-'9']['0'-'9']*) | ['0'-'9'])

rule token = parse
  | "//" [^ '\n' '\r']+ { token lexbuf }
  | "\r\n" { new_line lexbuf; token lexbuf }
  | "\n" { new_line lexbuf; token lexbuf }
  | "/*" { block_comment lexbuf }
  | blank+ { token lexbuf }
  | eof { EOF }
  | decimal as n { INT (int_of_string n) }
  | "==" { EQEQ }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ";" { SEMI }
  | "=" { EQ }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | ">" { GT }
  | "<" { LT }
  | "!" { BANG }
  | "&&" { AMPAMP }
  | "||" { PIPEPIPE }
  | "skip" { SKIP }
  | "abort" { ABORT }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "invariant" { INVARIANT }
  | "true" { TRUE }
  | "false" { FALSE }
  | "assert" { ASSERT }
  | "requires" { REQUIRES }
  | "ensures" { ENSURES }
  | eof { EOF }
  | id as x { ID x }

and block_comment = parse
  | "*/" { token lexbuf }
  | "*" { block_comment lexbuf }
  | "\r\n" { new_line lexbuf; block_comment lexbuf }
  | "\n" { new_line lexbuf; block_comment lexbuf }
  | ([^ '*' '\n'])+  { block_comment lexbuf }
