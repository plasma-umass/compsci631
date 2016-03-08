%{
open Imp_syntax
%}

%token LPAREN RPAREN
%token<string> STRING
%token<string> SYMBOL
%token<string> KEYWORD
%token<int> INT
%token EOF

%start sexp
%type <Smtlib_syntax.sexp> sexp

%%

sexp_list :
  | { [] }
  | sexp sexp_list { $1 :: $2 }

sexp :
  | INT { SInt $1 }
  | STRING { SString $1 }
  | SYMBOL { SSymbol $1 }
  | KEYWORD { SKeyword $1 }
  | LPAREN sexp_list RPAREN { SList $2 }

%%
