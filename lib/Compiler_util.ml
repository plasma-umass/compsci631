module IdSet = Set.Make (String)

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
  | Fun of id * id list * exp
  | App of exp * exp list
  | MkArray of exp * exp
  | GetArray of exp * exp
  | SetArray of exp * exp * exp
  | Seq of exp * exp
  | Abort
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
    parens exp <|>
    (symbol "abort" |>> fun _ -> Abort) <|>  
    (symbol "true" |>> fun _ -> Const (Bool true)) <|>
    (symbol "false" |>> fun _ -> Const (Bool false)) <|>
    (decimal |>> (fun n -> Const (Int n))) <|>
    (pipe2 (symbol "array" >> symbol "(" >> exp)
       (between (symbol ",") (symbol ")") exp)
       (fun e1 e2 -> MkArray (e1, e2))) <|>
    (id |>> (fun x -> Id x))
    ) s

  and get' e s = (
    (symbol "[" >> exp << symbol "]" >>= fun ix ->
     (symbol "=" >> exp |>> fun e' -> (SetArray (e, ix, e'))) <|>
     (get' (GetArray (e, ix)))) <|>
    (return e)) s
                  
  and get s = (atom >>= fun e -> get' e) s

  (* f g h = (f g) h *)
  and app' e s = (
    ( parens (comma_sep exp) >>= fun args ->
      app' (App (e, args)))  
    <|> (return e)) s
                               
  and app s = (
    get >>= fun f ->
    (parens (comma_sep exp) >>= fun args -> app' (App (f, args)))
    <|> return f) s

  and cmp s = expression operators app s

  and exp s = (
    (pipe3 (symbol "if" >> exp) (symbol "then" >> exp) (symbol "else" >> exp)
       (fun e1 e2 e3 -> If (e1, e2, e3))) 
    <|> (pipe3 (symbol "let" >> id) (symbol "=" >> exp) (symbol "in" >> exp)
               (fun x e1 e2 -> Let (x, e1, e2)))
    <|> (pipe3 (symbol "fun" >> id)
               (parens (comma_sep id))
               (symbol "->" >> exp)
               (fun f xs e -> Fun (f, xs, e)))
    <|> cmp
    ) s
end

let from_string = Generic_parser.from_string Parser.exp
let from_file = Generic_parser.from_file Parser.exp

type aexp =
  | AId of id
  | AConst of const
  [@@deriving show]

 and bexp =
  | BFun of id * id list * anfexp
  | BAtom of aexp
  | BOp2 of op2 * aexp * aexp
  | BMkArray of aexp * aexp
  | BGetArray of aexp * aexp
  | BSetArray of aexp * aexp * aexp
  | BApp of aexp * aexp list
  [@@deriving show]

and anfexp =
  | ELet of id * bexp * anfexp
  | EIf of aexp * anfexp * anfexp
  | EApp of aexp * aexp list
  | ERet of aexp
  | EAbort
  [@@deriving show]

let rec free_vars (exp : anfexp) : IdSet.t = match exp with
  | ELet (x, e1, e2) -> IdSet.union (free_vars_bexp e1) (IdSet.remove x (free_vars e2))
  | EIf (e1, e2, e3) -> IdSet.union (free_vars_aexp e1) (IdSet.union (free_vars e2) (free_vars e3))
  | EApp (f, es) -> List.fold_left IdSet.union (free_vars_aexp f) (List.map free_vars_aexp es)
  | ERet a -> free_vars_aexp a
  | EAbort -> IdSet.empty

and free_vars_bexp (exp : bexp) : IdSet.t = match exp with
  | BFun (f, xs, e) -> IdSet.diff (free_vars e) (IdSet.of_list (f :: xs))
  | BAtom e -> free_vars_aexp e
  | BOp2 (_, e1, e2)
  | BMkArray (e1, e2)
  | BGetArray (e1, e2) -> IdSet.union (free_vars_aexp e1) (free_vars_aexp e2)
  | BSetArray (e1, e2, e3) -> IdSet.union (IdSet.union (free_vars_aexp e1) (free_vars_aexp e2)) (free_vars_aexp e3)
  | BApp (f, es) -> List.fold_left IdSet.union (free_vars_aexp f) (List.map free_vars_aexp es)

and free_vars_aexp (exp : aexp) : IdSet.t = match exp with
  | AId x -> IdSet.singleton x
  | AConst _ -> IdSet.empty

let rec rename (x : id) (y : id) (exp : anfexp) : anfexp = match exp with
  | ELet (z, e1, e2) -> ELet (z, rename_bexp x y e1, if z = x then e2 else rename x y e2)
  | EIf (e1, e2, e3) -> EIf (rename_aexp x y e1, rename x y e2, rename x y e3)
  | EApp (f, es) -> EApp (rename_aexp x y f, List.map (rename_aexp x y) es)
  | ERet a -> ERet (rename_aexp x y a)
  | EAbort -> exp

and rename_bexp (x : id) (y : id) (exp : bexp) : bexp = match exp with
  | BFun (f, zs, e) ->
     if List.mem x (f :: zs) then BFun (f, zs, e)
     else if List.mem y zs then assert false (* need capture-free substitution *)
     else BFun (f, zs, rename x y e)
  | BAtom e -> BAtom (rename_aexp x y e)
  | BOp2 (op, e1, e2) -> BOp2 (op, rename_aexp x y e1, rename_aexp x y e2)
  | BMkArray (e1, e2) -> BMkArray (rename_aexp x y e1, rename_aexp x y e2)
  | BGetArray (e1, e2) -> BGetArray (rename_aexp x y e1, rename_aexp x y e2)
  | BSetArray (e1, e2, e3) -> BSetArray (rename_aexp x y e1, rename_aexp x y e2, rename_aexp x y e3)
  | BApp (f, es) -> BApp (rename_aexp x y f, List.map (rename_aexp x y) es)

and rename_aexp (x : id) (y : id) (exp : aexp) : aexp = match exp with
  | AId z -> AId (if z = x then y else z)
  | AConst c -> AConst c
