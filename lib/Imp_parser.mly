%{
open Imp_syntax
%}

%token SKIP ABORT IF ELSE WHILE SEMI INVARIANT LBRACE RBRACE LPAREN RPAREN
%token PLUS MINUS STAR LT GT EQ BANG AMPAMP PIPEPIPE EQEQ LEQ GEQ
%token TRUE FALSE ASSERT REQUIRES ENSURES
%token EOF
%token<string> ID
%token<int> INT

%start program
%type <Imp_syntax.bexp * Imp_syntax.cmd * Imp_syntax.bexp> program

%%
aatom :
  | INT { AConst $1 }
  | ID { AVar $1 }
  | LPAREN aexp RPAREN { $2 }

 mul :
   | aatom { $1 }
   | mul STAR aatom { AOp (Mul, $1, $3) }

add :
  | mul { $1 }
  | add MINUS mul { AOp (Sub, $1, $3) }
  | add PLUS mul { AOp (Add, $1, $3) }

aexp :
  | add { $1 }

batom :
  | TRUE { BConst true }
  | FALSE { BConst false }
  | LPAREN bexp RPAREN { $2 }
  | BANG batom { BNot $2 }
  | aexp LT aexp { BCmp (Lt, $1, $3) }
  | aexp GT aexp { BCmp (Gt, $1, $3) }
  | aexp LEQ aexp { BCmp (Lte, $1, $3) }
  | aexp GEQ aexp { BCmp (Gte, $1, $3) }
  | aexp EQEQ aexp { BCmp (Eq, $1, $3) }

and_ :
  | batom { $1 }
  | and_ AMPAMP batom { BAnd ($1, $3) }

or_ :
  | and_ { $1 }
  | or_ PIPEPIPE and_ { BOr ($1, $3) }

bexp :
  | or_ { $1 }

acmd :
  | SKIP SEMI { CSkip }
  | ABORT SEMI { CAbort }
  | ID EQ aexp SEMI { CAssign ($1, $3) }
  | LBRACE cmd RBRACE { $2 }
  | IF LPAREN bexp RPAREN acmd ELSE acmd { CIf ($3, $5, $7) }
  | WHILE bexp INVARIANT bexp acmd { CWhile ($2, $4, $5) }
  | ASSERT bexp SEMI { CIf ($2, CSkip,CAbort) }

cmd :
  | acmd { $1 }
  | acmd cmd { CSeq($1, $2) }

program :
  | REQUIRES bexp SEMI ENSURES bexp SEMI cmd EOF { ($2, $7, $5) }
  | ENSURES bexp SEMI cmd EOF { (BConst true, $4, $2) }
  | REQUIRES bexp SEMI cmd EOF { ($2, $4, BConst true) }
  | cmd EOF { (BConst true, $1, BConst true) }
%%
