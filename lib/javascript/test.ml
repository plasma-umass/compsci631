open JavaScript_syntax

let _ =
  match JavaScript.parse_javascript "1+2;" "test" with
    | Prog(ExprStmt(InfixExpr(_, _, _)) :: _) as p ->
        print_endline (FormatExt.to_string JavaScript.Pretty.p_prog p)
    | _ -> failwith "no beuno"

let _ = print_endline (show_prog (JavaScript.parse_javascript "1+2;" "test"))
