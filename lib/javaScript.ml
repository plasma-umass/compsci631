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
  | Prog of stmt list

open Lexing

let from_string cin =
  let lexbuf = Lexing.from_string cin in
    try
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" };
      JavaScript_parser.program JavaScript_lexer.token lexbuf
    with
      |  Failure msg ->
           failwith msg
      | JavaScript_parser.Error ->
           failwith (Printf.sprintf "parse error; unexpected token %s"
                       (lexeme lexbuf))

let from_file filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
    try
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      JavaScript_parser.program JavaScript_lexer.token lexbuf
    with
      |  Failure msg ->
           failwith msg
      | JavaScript_parser.Error ->
           failwith (Printf.sprintf "parse error; unexpected token %s"
                       (lexeme lexbuf))

let parse_expr cin name =
  let lexbuf = Lexing.from_string cin in
    try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      JavaScript_parser.expression JavaScript_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (Printf.sprintf "lexical error")
      | JavaScript_parser.Error ->
           failwith (Printf.sprintf "parse error")


module Pretty = struct

  open Format
  open FormatExt

  let p_const e = match e with
    | CString s -> text  ("\"" ^ String.escaped s ^ "\"")
    | CRegexp (re, _, _) -> text ("/" ^ re ^ "/")
    | CNum f -> squish [ (fun fmt -> pp_print_float fmt f); text "f" ]
    | CInt n -> int n
    | CBool b -> fun fmt -> pp_print_bool fmt b
      | CNull -> text "#null"
      | CUndefined -> text "#undefined"

  let prefixOp (op : prefixOp) = match op with
      PrefixLNot -> text "!"
    | PrefixBNot -> text "~"
    | PrefixPlus -> text "+"
    | PrefixMinus -> text "-"
    | PrefixTypeof -> text "typeof"
    | PrefixVoid -> text "void"
    | PrefixDelete -> text "delete"


  let unaryAssignOp (op : unaryAssignOp) = match op with
      PrefixInc -> text "++"
    | PrefixDec -> text "--"
    | PostfixInc -> text "++"
    | PostfixDec -> text "--"

  let prefix_unaryAssignOp op = match op with
      PrefixInc -> true
    | PrefixDec -> true
    | PostfixInc -> false
    | PostfixDec -> false


  let infixOp (op : infixOp) = match op with
      OpLT -> text "<"
    | OpLEq -> text "<="
    | OpGT -> text ">"
    | OpGEq -> text ">="
    | OpIn -> text "in"
    | OpInstanceof -> text "instanceof"
    | OpEq -> text "=="
    | OpNEq -> text "!="
    | OpStrictEq -> text "==="
    | OpStrictNEq -> text "!=="
    | OpLAnd -> text "&&"
    | OpLOr -> text "||"
    | OpMul -> text "*"
    | OpDiv -> text "/"
    | OpMod -> text "%"
    | OpSub -> text "-"
    | OpLShift -> text "<<"
    | OpSpRShift -> text ">>>"
    | OpZfRShift -> text ">>"
    | OpBAnd -> text "&"
    | OpBXor -> text "^"
    | OpBOr -> text "|"
    | OpAdd -> text "+"

  let assignOp (op : assignOp) = match op with
      OpAssign -> text "="
    | OpAssignAdd -> text "+="
    | OpAssignSub -> text "-="
    | OpAssignMul -> text "*="
    | OpAssignDiv -> text "/="
    | OpAssignMod -> text "%="
    | OpAssignLShift -> text "<<="
    | OpAssignSpRShift -> text ">>>="
    | OpAssignZfRShift -> text ">>="
    | OpAssignBAnd -> text "&="
    | OpAssignBXor -> text "^="
    | OpAssignBOr -> text "|="


  let string s = text ("\"" ^ s ^ "\"") (* TODO: fix escapes *)

  let rec commas (ps : printer list) = match ps with
      [] -> []
    | [p] -> [p]
    | p1 :: p2 :: ps ->
        (fun fmt ->
           pp_open_vbox fmt 0 ;
           p1 fmt;
           pp_print_string fmt ",";
           pp_close_box fmt ()) :: commas (p2 :: ps)

  let prop (p : prop) = match p with
      PropId x -> text x
    | PropString s -> string s
    | PropNum n -> int n


  let rec varDecl decl = match decl with
      VarDeclNoInit (x) -> text x
    | VarDecl (x,e) -> horz [text x; text "="; expr e]

  and caseClause clause = match clause with
      CaseClause (e,s) -> sep [expr e; text ":"; stmt s]
    | CaseDefault (s) -> sep [text "default:"; stmt s]

  and block s = match s with
      BlockStmt _ -> stmt s
    | _ -> vert [ text "{"; nest (stmt s); text "}" ]

  and paren_exp e = match e with
      ParenExpr _ -> expr e
    | _ -> sep [ text "("; expr e; text ")" ]


  and for_init fi = match fi with
      NoForInit -> text ""
    | VarForInit decls -> sep [ text "var";
                                vert (commas (List.map varDecl decls)) ]
    | ExprForInit e -> expr e


  and for_in_init fii = match fii with
      VarForInInit (x) -> sep [text "var"; text x]
    | NoVarForInInit (x) -> text x


  and lvalue lv = match lv with
      VarLValue (x) -> text x
    | DotLValue (e,x) -> squish [expr e; text "."; text x]
    | BracketLValue (e1,e2) -> squish [expr e1; brackets [expr e2]]


  and catch clause = match clause with
      CatchClause (x,s) ->
        vert [ sep [ text "catch"; parens [text x] ]; block s ]


  and expr e = match e with
    | ConstExpr (c) -> begin match c with
        | CUndefined -> text ""
        | c -> p_const c
      end
    | ArrayExpr (es) -> brackets [horz (commas (List.map expr es))]
    | ObjectExpr (ps) ->
        let f ( p, e) = sep [prop p; text ":"; expr e]
        in vert [ text "{"; nest (vert (List.map f ps)); text "}" ]
    | ThisExpr -> text "this"
    | VarExpr (x) -> text x
    | DotExpr (e,x) -> squish [expr e; text "."; text x]
    | BracketExpr (e1,e2) -> squish [expr e1; text "["; expr e2; text "]"]
    | NewExpr (constr,args) ->
        squish [text "new "; expr constr;
                parens [horz (commas (List.map expr args))] ]
    | PrefixExpr (op,e) -> sep [prefixOp op; expr e]
    | UnaryAssignExpr ( op, lv) ->
        if prefix_unaryAssignOp op
        then sep [ unaryAssignOp op; lvalue lv ]
        else sep [ lvalue lv; unaryAssignOp op ]
    | InfixExpr (op,e1,e2) ->
        sep [expr e1; infixOp op; expr e2]
    | IfExpr (e1,e2,e3) ->
        sep [expr e1; text "?"; expr e2; text ":"; expr e3]
    | AssignExpr (op,lv,e) -> sep [lvalue lv; assignOp op; expr e]
    | ParenExpr (e) -> parens [expr e]
    | ListExpr (e1,e2) -> sep (commas [expr e1; expr e2 ])
    | CallExpr (func,args) ->
        squish [
          expr (match func with | FuncExpr _ -> ParenExpr func | _ -> func);
          parens [horz (commas (List.map expr args))] ]
    | FuncExpr (args,body) ->
        vert [ sep [ text "function"; parens [horz (commas (List.map text args))] ];
               block body ]
    | NamedFuncExpr (name,args,body) ->
        vert [ sep [ text "function"; text name;
                     parens [horz (commas (List.map text args))] ];
          block body ]

  and stmt s = match s with
    | BlockStmt (ss) ->
        vert [ text "{"; nest (vert (List.map stmt ss)); text "}" ]
    | EmptyStmt -> text ";"
    | ExprStmt e -> horz [ expr e; text ";" ]
    | IfStmt (e,s1,s2) ->
        vert [ sep [ text "if"; paren_exp e ]; stmt s1; text "else"; stmt s2 ]
    | IfSingleStmt (e,s1) -> vert [ sep [text "if"; paren_exp e ]; stmt s1 ]
    | SwitchStmt (e,clauses) ->
        vert [ horz [ text "switch"; paren_exp e ];
               braces [vert (List.map caseClause clauses)] ]
    | WhileStmt (e,s) -> vert [ sep [ text "while"; paren_exp e ]; stmt s ]
    | DoWhileStmt (s,e) ->
        sep [text "do"; stmt s; text "while"; paren_exp e]
    | BreakStmt -> text "break;"
    | BreakToStmt (x) -> text ("break " ^ x ^ ";")
    | ContinueStmt -> text "continue;"
    | ContinueToStmt  (x) -> text ("continue " ^ x ^ ";")
    | LabelledStmt (x,s) -> sep [text (x ^ ":"); stmt s]
    | ForInStmt (fii,e,s) ->
        vert [ sep [ text "for";
                     parens [horz [ for_in_init fii; text "in "; expr e]] ];
               block s ]
    | ForStmt (fi,e1,e2,s) ->
        vert [ sep  [text "for"; parens [horz [ for_init fi; expr e1; expr e2 ]] ];
               stmt s ]
    | TryStmt (body,catches,EmptyStmt) ->
        vert (text "try" :: block body :: (List.map catch catches))
    | TryStmt (body,catches,finally) ->
        vert [ text "try"; block body; sep (List.map catch catches);
               text "finally"; block finally ]
    | ThrowStmt (e) -> sep [text "throw"; expr e; text ";"]
    | ReturnStmt (e) ->  horz [ text "return"; nest (expr e); text ";" ]
    | WithStmt (e,s) -> sep [text "with"; paren_exp e; stmt s]
    | VarDeclStmt (decls) ->
        squish [ text "var "; horz (commas (List.map varDecl decls)); text ";" ]
    | FuncStmt (name,args,body) ->
        sep [text "function"; text name;
             parens [horz (commas (List.map text args))];
             block body]

  let p_expr = expr

  let p_stmt = stmt

  let p_prog (Prog (ss)) = vert (List.map p_stmt ss)

  let p_infixOp = infixOp

  let p_prefixOp = prefixOp
end

let show_expr = FormatExt.to_string Pretty.p_expr
let show_stmt = FormatExt.to_string Pretty.p_stmt
let show_prog = FormatExt.to_string Pretty.p_prog