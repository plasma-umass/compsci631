type id = string [@@deriving show]

type op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  [@@deriving show]

type const =
  | Int of int
  | Bool of bool
  [@@deriving show]

type exp =
  | Id of id
  | Const of const
  | Op2 of op2 * exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * exp
  | Fix of id * exp
  | App of exp * exp
  | Empty
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | IsEmpty of exp
  | Record of (string * exp) list
  | GetField of exp * string
  [@@deriving show]

module Parser = struct
  open MParser
  open MParser_RE
  open Tokens
  open Generic_parser

  let infix sym op : (exp, unit) operator =
    let f e1 e2 = Op2 (op, e1, e2) in
    Infix  (skip_symbol sym >> return f, Assoc_left)

  let operators : (exp, unit) operator list list = [
    [infix "*" Mul; infix "/" Div; infix "%" Mod];
    [infix "+" Add; infix "-" Sub];
    [infix "==" Eq; infix ">" GT; infix "<" LT]
  ]

  let rec atom s  = (
    (id |>> (fun x -> Id x)) <|>
    parens exp <|>
    (symbol "true" |>> fun _ -> Const (Bool true)) <|>
    (symbol "false" |>> fun _ -> Const (Bool false)) <|>
    (symbol "empty" |>> fun _ -> Empty) <|>
    (decimal |>> (fun n -> Const (Int n))) <|>
    braces (comma_sep field |>> fun xs -> Record xs)
    ) s

  and field s =
    pipe2 id (symbol ":" >> exp) (fun x e -> (x, e)) s

  and get s = (
    pipe2 atom ((symbol "." >> (sep_by1 id (symbol "."))) <|> (return []))
      (fun e fields -> List.fold_left (fun x y -> GetField (x, y)) e fields)
  ) s

  and app s = (
    app_pattern get (fun x y -> App (x, y)) <|>
    (symbol "head" >> get |>> fun e -> (Head e)) <|>
    (symbol "tail" >> get |>> fun e -> (Tail e)) <|>
    (symbol "is_empty" >> get |>> fun e -> (IsEmpty e))) s

  and list s = (
    sep_by1 app (symbol "::") |>>
    rev_fold_left (fun x y -> Cons (y, x))
    ) s

  and cmp s = expression operators list s

  and exp s = (
    cmp <|>
    (pipe3 (symbol "if" >> exp) (symbol "then" >> exp) (symbol "else" >> exp)
       (fun e1 e2 e3 -> If (e1, e2, e3))) <|>
    (pipe3 (symbol "let" >> id) (symbol "=" >> exp) (symbol "in" >> exp)
       (fun x e1 e2 -> Let (x, e1, e2))) <|>
    (pipe2 (symbol "fun" >> id) (symbol "->" >> exp)
       (fun x e -> Fun (x, e))) <|>
    (pipe2 (symbol "fix" >> id) (symbol "->" >> exp)
       (fun x e -> Fix (x, e)))) s
end

let from_string = Generic_parser.from_string Parser.exp
let from_file = Generic_parser.from_file Parser.exp
