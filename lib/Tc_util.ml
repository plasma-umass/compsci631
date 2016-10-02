type id = string [@@deriving show]

type tid = string [@@deriving show]

type metavar = string [@@deriving show]
                      
type typ =
  | TBool
  | TInt
  | TFun of typ * typ
  | TList of typ
  | TRecord of (string * typ) list
  | TArr of typ
  | TForall of tid * typ
  | TId of tid
  | TMetavar of string             
  [@@deriving show]

type op2 = Xinterp_util.op2 = 
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  [@@deriving show]

type const = Xinterp_util.const =
  | Int of int
  | Bool of bool
  [@@deriving show]

type exp =
  | Id of id
  | Const of const
  | Op2 of op2 * exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * typ * exp
  | Fix of id * typ * exp
  | App of exp * exp
  | Empty of typ 
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | IsEmpty of exp
  | Record of (string * exp) list
  | GetField of exp * string
  | MkArray of exp * exp
  | GetArray of exp * exp
  | SetArray of exp * exp * exp
  | TypFun of tid * exp
  | TypApp of exp * typ
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

  let rec typ_atom s = (
    (id |>> fun x -> TId x)
    <|> parens typ
    <|> (symbol "bool" |>> fun _ -> TBool)
    <|> (symbol "int" |>> fun _ -> TInt) 
    <|> braces (comma_sep typ_field |>> fun xs -> TRecord xs)
    <|> brackets (typ  |>> fun t -> TArr t)) s

  and typ_list_array' acc_t s = (
    (symbol "list" >>= fun _ -> typ_list_array' (TList acc_t))
    <|> (symbol "array" >>= fun _ -> typ_list_array' (TArr acc_t))
    <|> (return acc_t)) s

  and typ_list_array s = (typ_atom >>= fun acc_t -> typ_list_array' acc_t) s

  and typ_fun s = (
    typ_list_array >>= fun t1 ->
    ((symbol "->" >> typ_fun |>> fun t2 -> TFun (t1, t2))
     <|>  (return t1))) s
               
  and typ s = (
    (pipe2 (symbol "forall" >> id) (symbol "." >> typ)
           (fun x t -> TForall (x, t)))
    <|> typ_fun) s
     
  and typ_field s =
    pipe2 id (symbol ":" >> typ) (fun x t -> (x, t)) s
                        
  let rec atom s  = (
    (id |>> (fun x -> Id x)) <|>
    parens exp <|>
    (symbol "true" |>> fun _ -> Const (Bool true)) <|>
    (symbol "false" |>> fun _ -> Const (Bool false)) <|>
    (symbol "empty" >> brackets typ |>> fun t -> Empty t) <|>
    (pipe2 (symbol "array" >> symbol "(" >> exp)
           (symbol "," >> exp  << symbol ")")
           (fun e1 e2 -> MkArray (e1, e2))) <|>
    (decimal |>> (fun n -> Const (Int n))) <|>
    braces (comma_sep field |>> fun xs -> Record xs)
    ) s

  and field s =
    pipe2 id (symbol ":" >> exp) (fun x e -> (x, e)) s

  (* obj.x.y = (obj.x).y *)
  and get' e s = (
    (symbol "." >> id >>= fun x -> get' (GetField (e, x))) <|>
    (symbol "[" >> exp << symbol "]" >>= fun ix ->
     (symbol "=" >> exp |>> fun e' -> (SetArray (e, ix, e'))) <|>
     (get' (GetArray (e, ix)))) <|>
    (return e)) s
                  
  and get s = (atom >>= fun e -> get' e) s

  (* f g h = (f g) h *)
  and app' e s = (
    (brackets typ >>= fun t -> app' (TypApp (e, t)))
    <|> (get >>= fun e' -> app' (App (e, e')))
    <|> (return e)) s
                               
  and app s = (
    (symbol "head" >> get |>> fun e -> (Head e)) <|>
    (symbol "tail" >> get |>> fun e -> (Tail e)) <|>
    (symbol "is_empty" >> get |>> fun e -> (IsEmpty e)) <|>
    (get >>= fun e -> app' e)) s

  and list s = (
    sep_by1 app (symbol "::") |>>
    rev_fold_left (fun x y -> Cons (y, x))
    ) s

  and cmp s = expression operators list s

  and exp s = (
    cmp 
    <|> (pipe3 (symbol "if" >> exp) (symbol "then" >> exp) (symbol "else" >> exp)
       (fun e1 e2 e3 -> If (e1, e2, e3))) 
    <|> (pipe3 (symbol "let" >> id) (symbol "=" >> exp) (symbol "in" >> exp)
               (fun x e1 e2 -> Let (x, e1, e2)))
    <|> (pipe2 (symbol "tfun" >> id)
               (symbol "." >> exp)
               (fun x e -> TypFun (x, e)))
    <|> (pipe3 (symbol "fun" >> symbol "(" >> id)
               (symbol ":" >> typ)
               (symbol ")" >> symbol "->" >> exp)
               (fun x t e -> Fun (x, t, e)))
    <|> (pipe3 (symbol "fix" >> symbol "(" >> id)
               (symbol ":" >> typ)
               (symbol ")" >> symbol "->" >> exp)
               (fun x t e -> Fix (x, t, e)))
    ) s
end

let from_string = Generic_parser.from_string Parser.exp
let from_file = Generic_parser.from_file Parser.exp
