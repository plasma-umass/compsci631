(** Parser and printer for JavaScript *)

type id = string

type prefixOp = JavaScript_syntax.prefixOp =
  | PrefixLNot
  | PrefixBNot
  | PrefixPlus
  | PrefixMinus
  | PrefixTypeof
  | PrefixVoid
  | PrefixDelete

type unaryAssignOp = JavaScript_syntax.unaryAssignOp =
  | PrefixInc
  | PrefixDec
  | PostfixInc
  | PostfixDec

type infixOp = JavaScript_syntax.infixOp =
  | OpLT
  | OpLEq
  | OpGT
  | OpGEq
  | OpIn
  | OpInstanceof
  | OpEq
  | OpNEq
  | OpStrictEq
  | OpStrictNEq
  | OpLAnd
  | OpLOr
  | OpMul
  | OpDiv
  | OpMod
  | OpSub
  | OpLShift
  | OpSpRShift
  | OpZfRShift
  | OpBAnd
  | OpBXor
  | OpBOr
  | OpAdd

type assignOp = JavaScript_syntax.assignOp =
  | OpAssign
  | OpAssignAdd
  | OpAssignSub
  | OpAssignMul
  | OpAssignDiv
  | OpAssignMod
  | OpAssignLShift
  | OpAssignSpRShift
  | OpAssignZfRShift
  | OpAssignBAnd
  | OpAssignBXor
  | OpAssignBOr

type const = JavaScript_syntax.const =
  | CString of string
  | CRegexp of string * bool * bool
  | CNum of float
  | CInt of int
  | CBool of bool
  | CNull
  | CUndefined

type prop = JavaScript_syntax.prop =
  | PropId of id
  | PropString of string
  | PropNum of int

type varDecl = JavaScript_syntax.varDecl =
  | VarDeclNoInit of  id
  | VarDecl of  id * expr

and forInit = JavaScript_syntax.forInit =
  | NoForInit
  | VarForInit of varDecl list
  | ExprForInit of expr

and catch = JavaScript_syntax.catch =
  | CatchClause of  id * stmt

and forInInit = JavaScript_syntax.forInInit =
  | VarForInInit of id
  | NoVarForInInit of id

and caseClause = JavaScript_syntax.caseClause =
  | CaseClause of  expr * stmt
  | CaseDefault of  stmt

and lvalue = JavaScript_syntax.lvalue =
  | VarLValue of  id
  | DotLValue of  expr * id
  | BracketLValue of  expr * expr

and expr = JavaScript_syntax.expr =
  | ConstExpr of  const
  | ArrayExpr of  expr list
  | ObjectExpr of  (prop * expr) list
  | ThisExpr
  | VarExpr of  id
  | DotExpr of  expr * id
  | BracketExpr of  expr * expr
  | NewExpr of  expr * expr list
  | PrefixExpr of  prefixOp * expr
  | UnaryAssignExpr of  unaryAssignOp * lvalue
  | InfixExpr of  infixOp * expr * expr
  | IfExpr of  expr * expr * expr
  | AssignExpr of  assignOp * lvalue * expr
  | ParenExpr of  expr
  | ListExpr of  expr * expr
  | CallExpr of  expr * expr list
  | FuncExpr of  id list * stmt
  | NamedFuncExpr of  id * id list * stmt

and stmt = JavaScript_syntax.stmt =
  | BlockStmt of  stmt list
  | EmptyStmt
  | ExprStmt of expr
  | IfStmt of  expr * stmt * stmt
  | IfSingleStmt of  expr * stmt
  | SwitchStmt of  expr * caseClause list
  | WhileStmt of  expr * stmt
  | DoWhileStmt of  stmt * expr
  | BreakStmt
  | BreakToStmt of  id
  | ContinueStmt
  | ContinueToStmt of  id
  | LabelledStmt of  id * stmt
  | ForInStmt of  forInInit * expr * stmt
  | ForStmt of  forInit * expr * expr * stmt
  | TryStmt of  stmt * catch list * stmt
  | ThrowStmt of  expr
  | ReturnStmt of  expr
  | WithStmt of  expr * stmt
  | VarDeclStmt of  varDecl list
  | FuncStmt of  id * id list * stmt

type prog = JavaScript_syntax.prog =
  | Prog of  stmt list

val from_string : string -> prog
val from_file : string -> prog

val show_expr : expr -> string
val show_stmt : stmt -> string
val show_prog : prog -> string