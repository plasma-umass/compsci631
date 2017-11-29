%{
(** A JavaScript parser that does not do semicolon insertion. *)
open Prelude
open JavaScript_syntax

exception Expected_lvalue

exception Parse_failure of string

let rec expr_to_lvalue (e : expr) : lvalue =  match e with
  | VarExpr (x) -> VarLValue (x)
  | DotExpr (e,x) -> DotLValue (e,x)
  | BracketExpr (e1,e2) -> BracketLValue (e1,e2)
  | ParenExpr (e) -> expr_to_lvalue e
  | _ -> raise Expected_lvalue
%}

%token <string> ContinueId
%token <string> BreakId
%token <string> Id
%token <string> String
%token <string * bool * bool> Regexp
%token <int> Int
%token <float> Float
%token <JavaScript_syntax.assignOp> AssignOp

%token If Else True False New Instanceof This Null Function Typeof Void
 Delete Switch Default Case While Do Break Var In For Try Catch Finally Throw
 Return With Continue

%token LBrace RBrace LParen RParen Assign
 Semi Comma Ques Colon LOr LAnd BOr BXor BAnd StrictEq AbstractEq
 StrictNEq AbstractNEq LShift RShift SpRShift LEq LT GEq GT PlusPlus MinusMinus
 Plus Minus Times Div Mod Exclamation Tilde Period LBrack RBrack

%token EOF

(* http://stackoverflow.com/questions/1737460/
   how-to-find-shift-reduce-conflict-in-this-yacc-file *)
%nonassoc GreaterThanColon
%nonassoc Colon
%nonassoc LowerThanElse
%nonassoc Else

%left LOr
%left LAnd
%left BOr
%left BXor
%left BAnd
%left StrictEq StrictNEq AbstractEq AbstractNEq
%left LT LEq GT GEq In Instanceof
%left LShift RShift SpRShift
%left Plus Minus
%left Times Div Mod


%start program
%start expression

%type <JavaScript_syntax.prog> program
%type <JavaScript_syntax.expr> expression

%%

exprs
  : { [] }
  | assign_expr { [$1] }
  | assign_expr Comma exprs { $1::$3 }

stmts
  : { [] }
  | stmt stmts { $1 :: $2 }

cases
  : { [] }
  | case cases { $1 :: $2 }

catches
  : { [] }
  | catch catches { $1 :: $2 }

ids
  : { [] }
  | Id { [$1] }
  | Id Comma ids { $1 :: $3 }

prop
  : Id { PropId $1 }  %prec GreaterThanColon
  | String { PropString $1 }

fields
  : { [] }
  | prop Colon expr
    { [ ($1, $3) ] }
  | prop Colon expr Comma fields
      { ($1, $3) :: $5 }

varDecls
  : varDecl { [$1] }
  | varDecl Comma varDecls { $1::$3 }

varDecls_noin
  : varDecl_noin { [$1] }
  | varDecl_noin Comma varDecls_noin { $1::$3 }

element_list
  :
      { [] }
  | Comma
      { [ ConstExpr (CUndefined) ] }
  | assign_expr { [$1] }
  | assign_expr Comma element_list
      { $1::$3 }

const :
  | True { CBool true }
  | False { CBool false }
  | Null { CNull }
  | String { CString $1 }
  | Regexp { let re, g, ci = $1 in  CRegexp (re, g, ci) }
  | Int { CInt $1}
  | Float { CNum $1 }

primary_expr :
  | const { ConstExpr ($1) }
  | Id { VarExpr ($1) }
  | LBrack element_list RBrack
      { ArrayExpr ($2) }
  | LBrace fields RBrace
      { ObjectExpr ($2) }
  | LParen expr RParen
      { ParenExpr ($2) }
  | This { ThisExpr }

member_expr
  : primary_expr
      { $1 }
  | Function LParen ids RParen body=src_elt_block
    { FuncExpr ( $3, body) }
  | Function Id LParen ids RParen body=src_elt_block
    { NamedFuncExpr ( $2, $4, body) }
  | member_expr Period Id
      { DotExpr ( $1, $3) }
  | member_expr LBrack expr RBrack
      { BracketExpr ($1,$3) }
  | New member_expr LParen exprs RParen
      { NewExpr ($2,$4) }

new_expr
  : member_expr
      { $1 }
  | New new_expr
      { NewExpr ($2,[]) }


call_expr
  : member_expr LParen exprs RParen
      { CallExpr ($1,$3) }
  | call_expr LParen exprs RParen
      { CallExpr ($1,$3) }
  | call_expr LBrack expr RBrack
      { BracketExpr ($1,$3) }
  | call_expr Period Id
      { DotExpr ( $1, $3) }

lhs_expr
  : new_expr
      { $1 }
  | call_expr
      { $1 }

postfix_expr
  : lhs_expr
      { $1 }
  | lhs_expr PlusPlus
      { UnaryAssignExpr (PostfixInc,expr_to_lvalue $1) }
  | lhs_expr MinusMinus
      { UnaryAssignExpr (PostfixDec,expr_to_lvalue $1) }

unary_expr
  : postfix_expr
      { $1 }
  | PlusPlus unary_expr
      { UnaryAssignExpr (PrefixInc,expr_to_lvalue $2) }
  | MinusMinus unary_expr
      { UnaryAssignExpr (PrefixDec,expr_to_lvalue $2) }
  | Exclamation unary_expr
      { PrefixExpr (PrefixLNot,$2) }
  | Tilde unary_expr
      { PrefixExpr (PrefixBNot,$2) }
  | Minus unary_expr
      { PrefixExpr (PrefixMinus,$2) }
  | Plus unary_expr
      { PrefixExpr (PrefixPlus,$2) }
  | Typeof unary_expr
      { PrefixExpr (PrefixTypeof,$2) }
  | Void unary_expr
      { PrefixExpr (PrefixVoid,$2) }
  | Delete unary_expr
      { PrefixExpr (PrefixDelete,$2) }

(* Combines UnaryExpression, MultiplicativeExpression, AdditiveExpression, and
   ShiftExpression by using precedence and associativity rules. *)
op_expr
  : unary_expr { $1 }
  | op_expr Times op_expr
      { InfixExpr (OpMul,$1,$3) }
  | op_expr Div op_expr
      { InfixExpr (OpDiv,$1,$3) }
  | op_expr Mod op_expr
      { InfixExpr (OpMod,$1,$3) }
  | op_expr Plus op_expr
      { InfixExpr (OpAdd,$1,$3) }
  | op_expr Minus op_expr
      { InfixExpr (OpSub,$1,$3) }
  | op_expr LShift op_expr
      { InfixExpr (OpLShift,$1,$3) }
  | op_expr RShift op_expr
      { InfixExpr (OpZfRShift,$1,$3) }
  | op_expr SpRShift op_expr
      { InfixExpr (OpSpRShift,$1,$3) }

in_expr
  : op_expr
      { $1 }
  | in_expr LT in_expr
      { InfixExpr (OpLT,$1,$3) }
  | in_expr GT in_expr
      { InfixExpr (OpGT,$1,$3) }
  | in_expr LEq in_expr
      { InfixExpr (OpLEq,$1,$3) }
  | in_expr GEq in_expr
      { InfixExpr (OpGEq,$1,$3) }
  | in_expr Instanceof in_expr
      { InfixExpr (OpInstanceof,$1,$3) }
  | in_expr In in_expr
      { InfixExpr (OpIn,$1,$3) }
  | in_expr StrictEq in_expr
      { InfixExpr (OpStrictEq,$1,$3) }
  | in_expr StrictNEq in_expr
      { InfixExpr (OpStrictNEq,$1,$3) }
  | in_expr AbstractEq in_expr
      { InfixExpr (OpEq,$1,$3) }
  | in_expr AbstractNEq in_expr
      { InfixExpr (OpNEq,$1,$3) }
  | in_expr BAnd in_expr
      { InfixExpr (OpBAnd,$1,$3) }
  | in_expr BXor in_expr
      { InfixExpr (OpBXor,$1,$3) }
  | in_expr BOr in_expr
      { InfixExpr (OpBOr,$1,$3) }
  | in_expr LAnd in_expr
      { InfixExpr (OpLAnd,$1,$3) }
  | in_expr LOr in_expr
      { InfixExpr (OpLOr,$1,$3) }

cond_expr
  : in_expr
      { $1 }
  | in_expr Ques assign_expr Colon assign_expr
      { IfExpr ($1,$3,$5) }


assign_expr
  : cond_expr
      { $1 }
  (* we need the use Assign (token for =) in other productions. *)
  | lhs_expr AssignOp assign_expr
    { AssignExpr ( $2, expr_to_lvalue $1, $3) }
  | lhs_expr Assign assign_expr
    { AssignExpr ( OpAssign, expr_to_lvalue $1, $3) }


expr
  : assign_expr
      { $1 }
  | expr Comma assign_expr
      { ListExpr ($1,$3) }

noin_expr
  : op_expr
      { $1 }
  | noin_expr LT noin_expr
      { InfixExpr (OpLT,$1,$3) }
  | noin_expr GT noin_expr
      { InfixExpr (OpGT,$1,$3) }
  | noin_expr LEq noin_expr
      { InfixExpr (OpLEq,$1,$3) }
  | noin_expr GEq noin_expr
      { InfixExpr (OpGEq,$1,$3) }
  | noin_expr Instanceof noin_expr
      { InfixExpr (OpInstanceof,$1,$3) }
  | noin_expr StrictEq noin_expr
      { InfixExpr (OpStrictEq,$1,$3) }
  | noin_expr StrictNEq noin_expr
      { InfixExpr (OpStrictNEq,$1,$3) }
  | noin_expr AbstractEq noin_expr
      { InfixExpr (OpEq,$1,$3) }
  | noin_expr AbstractNEq noin_expr
      { InfixExpr (OpNEq,$1,$3) }
  | noin_expr BAnd noin_expr
      { InfixExpr (OpBAnd,$1,$3) }
  | noin_expr BXor noin_expr
      { InfixExpr (OpBXor,$1,$3) }
  | noin_expr BOr noin_expr
      { InfixExpr (OpBOr,$1,$3) }
  | noin_expr LAnd noin_expr
      { InfixExpr (OpLAnd,$1,$3) }
  | noin_expr LOr noin_expr
      { InfixExpr (OpLOr,$1,$3) }

cond_noin_expr
  : noin_expr { $1 }
  | noin_expr Ques assign_noin_expr Colon assign_noin_expr
    { IfExpr ($1,$3,$5) }

assign_noin_expr
  : cond_noin_expr { $1 }
  | lhs_expr AssignOp assign_noin_expr
    { AssignExpr ( $2, expr_to_lvalue $1, $3) }
  | lhs_expr Assign assign_noin_expr
    { AssignExpr ( OpAssign, expr_to_lvalue $1, $3) }

expr_noin
  : assign_noin_expr { $1 }
  | noin_expr Comma assign_noin_expr
      { ListExpr ($1,$3) }

varDecl
  : Id { VarDeclNoInit ($1) }
  | Id Assign assign_expr { VarDecl ($1,$3) }

varDecl_noin
  : Id { VarDeclNoInit ($1) }
  | Id Assign assign_noin_expr { VarDecl ($1,$3) }

case
  : Case expr Colon stmts
  { CaseClause ($2,BlockStmt ($4)) }
  | Default Colon stmts
  { CaseDefault (BlockStmt ($3)) }

forInInit :
  | Id
      { NoVarForInInit ( $1) }
  | Var Id
      { VarForInInit ( $2) }

forInit
  : { NoForInit }
  | Var varDecls_noin { VarForInit $2 }
  | expr_noin { ExprForInit $1 }

catch
  : Catch LParen Id RParen block
    { CatchClause ( $3, $5) }


block : LBrace stmts RBrace
      { BlockStmt ($2) }

paren_expr : LParen expr RParen
      { ParenExpr ($2) }

opt_expr :
  | { ConstExpr ( CUndefined) }
  | expr { $1 }

stmt :
  | LBrace stmts RBrace
      { BlockStmt ( $2) }
  | Semi
      { EmptyStmt }
  | expr Semi
      { match $1 with
          | NamedFuncExpr (x, args, body) -> FuncStmt (x, args, body)
          | e -> ExprStmt e
      }
  | Continue Semi
      { ContinueStmt }
  | ContinueId Semi
      { ContinueToStmt ($1) }
  | If LParen expr  RParen stmt  %prec LowerThanElse
    { IfSingleStmt ( $3, $5) }
  | If LParen expr RParen stmt Else stmt
    { IfStmt ( $3, $5, $7) }

  | Switch paren_expr LBrace cases RBrace
      { SwitchStmt ($2,$4) }
  | While paren_expr stmt
      { WhileStmt ($2,$3) }
  | Do block While paren_expr Semi
      { DoWhileStmt ($2,$4) }
  | Break  Semi
      { BreakStmt }
  | BreakId Semi
      { BreakToStmt ($1) }
  | Id Colon stmt { LabelledStmt ( $1, $3) }
  | For LParen forInInit In expr RParen stmt
    { ForInStmt ($3,$5,$7) }
  | For LParen forInit Semi opt_expr Semi opt_expr RParen stmt
    { ForStmt ($3,$5,$7,$9) }
  | Try block catches
    { TryStmt ($2,$3,EmptyStmt) }
  | Try block catches Finally block { TryStmt ($2,$3,$5) }
  | Throw expr Semi
      { ThrowStmt ($2) }
  | Return Semi
      { ReturnStmt (
                    ConstExpr ( CUndefined)) }
  | Return expr Semi
      { ReturnStmt ($2) }
  | Var varDecls Semi
      { VarDeclStmt ($2) }
  | With LParen expr RParen stmt
      { WithStmt ( $3, $5) }

src_elt_block
  : LBrace src_elts RBrace
      { BlockStmt ($2) }

src_elts
  : { [] }
  | src_elt src_elts { $1::$2 }

src_elt
  : stmt { $1 }
  | Function Id LParen ids RParen src_elt_block
    { FuncStmt ( $2, $4, $6) }

program : src_elts EOF { Prog ($1) }

expression : expr EOF { $1 }

%%
