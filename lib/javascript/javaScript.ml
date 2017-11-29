open JavaScript_syntax
open Lexing

let parse_javascript cin name =
  let lexbuf = Lexing.from_string cin in
    try
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      JavaScript_parser.program JavaScript_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (Printf.sprintf "lexical error")
      | JavaScript_parser.Error ->
           failwith (Printf.sprintf "parse error; unexpected token %s"
                       (lexeme lexbuf))

let parse_javascript_from_channel cin name =
  let lexbuf = Lexing.from_channel cin in
    try
      (* Set the correct filename in lexbuf (for source-tracking). *)
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };
      JavaScript_parser.program JavaScript_lexer.token lexbuf
    with
      |  Failure "lexing: empty token" ->
           failwith (Printf.sprintf "lexical error")
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
        squish [ expr func; parens [horz (commas (List.map expr args))] ]
    | FuncExpr (args,body) ->
        vert [ sep [ text "function"; parens [horz (commas (List.map text args))] ];
               stmt body ]
    | NamedFuncExpr (name,args,body) ->
        vert [ sep [ text "function"; text name;
                     parens [horz (commas (List.map text args))] ];
               stmt body ]

  and stmt s = match s with
    | BlockStmt (ss) ->
        vert [ text "{"; nest (vert (List.map stmt ss)); text "}" ]
    | EmptyStmt -> text ";"
    | ExprStmt e -> sep [ expr e; text ";" ]
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
    | ReturnStmt (e) ->  sep [ text "return"; nest (expr e); text ";" ]
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

