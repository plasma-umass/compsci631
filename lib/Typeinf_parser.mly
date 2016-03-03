%{

open Typeinf_syntax
open Implicit

let rec projn (e : exp) (n : int) : exp =
  if n < 1 then raise Parsing.Parse_error
  else if n = 1 then ProjL e
  else if n = 2 then ProjR e
  else projn (ProjL e) (n - 1)

%}

%token LPAREN RPAREN LANGLE RANGLE COMMA EQUALS DOT
%token PLUS MINUS STAR RARROW EMPTYQ
%token COLONCOLON
%token IF THEN ELSE LET IN EMPTY HEAD TAIL TRUE FALSE FUN FIX
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <Typeinf_syntax.Implicit.exp> program

%%

exps :
  | exp { $1 }
  | exps COMMA exp { Pair ($1, $3) }

atom :
  | INT { Const (Int $1) }
  | ID { Id $1 }
  | TRUE { Const (Bool true) }
  | FALSE { Const (Bool false) }
  | EMPTY { Empty }
  | LPAREN exps RPAREN { $2 }
  | atom DOT INT { projn $1 $3 }

app :
  | atom { $1 }
  | HEAD atom { Head $2 }
  | TAIL atom { Tail $2 }
  | EMPTYQ atom { IsEmpty $2 }
  | app atom { App ($1, $2) }

list_ :
  | app { $1 }
  | app COLONCOLON list_ { Cons ($1, $3) }

mul :
  | list_ { $1 }
  | mul STAR list_ { Op2 (Mul, $1, $3) }

add :
  | mul { $1 }
  | add MINUS mul { Op2 (Sub, $1, $3) }
  | add PLUS mul { Op2 (Add, $1, $3) }

cmp :
  | add { $1 }
  | add EQUALS add { Op2 (Eq, $1, $3) }
  | add LANGLE add { Op2 (LT, $1, $3) }
  | add RANGLE add { Op2 (GT, $1, $3) }

exp :
  | cmp { $1 }
  | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
  | LET ID EQUALS exp IN exp { Let ($2, $4, $6) }
  | FUN ID RARROW exp { Fun ($2, $4) }
  | FIX ID RARROW exp { Fix ($2, $4) }

program :
  | exp EOF { $1 }
%%
