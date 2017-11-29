type id = string
[@@deriving show]

type prefixOp =
  | PrefixLNot
  | PrefixBNot
  | PrefixPlus
  | PrefixMinus
  | PrefixTypeof
  | PrefixVoid
  | PrefixDelete
  [@@deriving show]

type unaryAssignOp =
  | PrefixInc
  | PrefixDec
  | PostfixInc
  | PostfixDec
  [@@deriving show]

type infixOp =
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
  [@@deriving show]

type assignOp =
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
  [@@deriving show]

type const =
  | CString of string
  | CRegexp of string * bool * bool
  | CNum of float
  | CInt of int
  | CBool of bool
  | CNull
  | CUndefined
  [@@deriving show]

type prop =
  | PropId of id
  | PropString of string
  | PropNum of int
  [@@deriving show]

type varDecl =
  | VarDeclNoInit of id
  | VarDecl of id * expr
  [@@deriving show]

and forInit =
  | NoForInit
  | VarForInit of varDecl list
  | ExprForInit of expr
  [@@deriving show]

and catch =
  | CatchClause of id * stmt
  [@@deriving show]

and forInInit =
  | VarForInInit of id
  | NoVarForInInit of id
  [@@deriving show]

and caseClause =
  | CaseClause of expr * stmt
  | CaseDefault of stmt
  [@@deriving show]

and lvalue =
  | VarLValue of id
  | DotLValue of expr * id
  | BracketLValue of expr * expr
  [@@deriving show]

and expr =
  | ConstExpr of const
  | ArrayExpr of expr list
  | ObjectExpr of (prop * expr) list
  | ThisExpr
  | VarExpr of id
  | DotExpr of expr * id
  | BracketExpr of expr * expr
  | NewExpr of expr * expr list
  | PrefixExpr of prefixOp * expr
  | UnaryAssignExpr of unaryAssignOp * lvalue
  | InfixExpr of infixOp * expr * expr
  | IfExpr of expr * expr * expr
  | AssignExpr of assignOp * lvalue * expr
  | ParenExpr of expr
  | ListExpr of expr * expr
  | CallExpr of expr * expr list
  | FuncExpr of id list * stmt
  | NamedFuncExpr of id * id list * stmt
  [@@deriving show]

and stmt =
  | BlockStmt of stmt list
  | EmptyStmt
  | ExprStmt of expr
  | IfStmt of expr * stmt * stmt
  | IfSingleStmt of expr * stmt
  | SwitchStmt of expr * caseClause list
  | WhileStmt of expr * stmt
  | DoWhileStmt of stmt * expr
  | BreakStmt
  | BreakToStmt of id
  | ContinueStmt
  | ContinueToStmt of id
  | LabelledStmt of id * stmt
  | ForInStmt of forInInit * expr * stmt
  | ForStmt of forInit * expr * expr * stmt
  | TryStmt of stmt * catch list * stmt
  | ThrowStmt of expr
  | ReturnStmt of expr
  | WithStmt of expr * stmt
  | VarDeclStmt of varDecl list
  | FuncStmt of id * id list * stmt
  [@@deriving show]

type prog =
  | Prog of stmt list
  [@@deriving show]
