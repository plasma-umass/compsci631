%{
open Imp_syntax
%}

%token SKIP ABORT IF ELSE WHILE SEMI INVARIANT LBRACE RBRACE LPAREN RPAREN
%token PLUS MINUS STAR LT GT EQ BANG AMPAMP PIPEPIPE
%token TRUE FALSE
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <Imp_syntax.cmd> program

%%
aatom :
  | INT { AConst $1 }
  | ID { AVar $1 }
  | LPAREN aexp RPAREN { $2 }

 mul :
   | aatom { $1 }
   | mul STAR aatom { AMul ($1, $3) }

add :
  | mul { $1 }
  | add PLUS mul { AAdd ($1, $3) }

aexp :
  | add { $1 }

batom :
  | TRUE { BConst true }
  | FALSE { BConst false }
  | LPAREN bexp RPAREN { $2 }
  | BANG batom { BNot $2 }
  | aexp LT aexp { BLT ($1, $3) }

and_ :
  | batom { $1 }
  | and_ AMPAMP batom { BAnd ($1, $3) }

or_ :
  | and_ { $1 }
  | or_ PIPEPIPE and_ { BOr ($1, $3) }

bexp :
  | or_ { $1 }

acmd :
  | SKIP { CSkip }
  | ABORT { CAbort }
  | ID EQ aexp { CAssign ($1, $3) }
  | LBRACE cmd RBRACE { $2 }
  | IF LPAREN bexp RPAREN acmd ELSE acmd { CIf ($3, $5, $7) }
  | WHILE bexp INVARIANT bexp acmd { CWhile ($2, $4, $5) }

cmd :
  | acmd { $1 }
  | acmd SEMI cmd { CSeq($1, $3) }

program :
  | cmd EOF { $1 }
%%
