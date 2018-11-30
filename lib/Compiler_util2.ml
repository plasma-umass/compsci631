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

type value =
  | Int of int
  | Bool of bool
  | Array of value array
  | Closure of (value list -> value)

let show_value (v : value) = match v with
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | Array _ -> "array"
    | Closure f -> Format.sprintf "<closure:%d>" (Obj.magic f)

module Interp = struct

  let bool (v : value) : bool = match v with
      | Bool b -> b
      | _ -> failwith "expected boolean value"

  let int (v : value) : int = match v with
      | Int n -> n
      | _ -> failwith "expected integer value"

  let array (v : value) : value array = match v with
      | Array a -> a
      | _ -> failwith "expected array value"

  let eval_op2 (op : op2) (v1 : value) (v2 : value) = match op with
      | LT -> Bool (int v1 < int v2)
      | GT -> Bool (int v1 > int v2)
      | Eq -> Bool (v1 = v2)
      | Add -> Int (int v1 + int v2)
      | Sub -> Int (int v1 - int v2)
      | Mul -> Int (int v1 * int v2)
      | Div -> Int (int v1 / int v2)
      | Mod ->Int (int v1 mod int v2)

  let rec eval (env : (id * value) list) (exp : exp) : value = match exp with
    | Id x -> List.assoc x env
    | Const (Int x) -> Int x
    | Const (Bool b) -> Bool b
    | Op2 (op, e1, e2) -> eval_op2 op (eval env e1) (eval env e2)
    | If (e1, e2, e3) ->
      if bool (eval env e1) then eval env e2 else eval env e3
    | Let (x, e1, e2) -> eval ((x, eval env e1) :: env) e2
    | Fun (f, formals, body) ->
        let rec closure = Closure (fun args ->
            let env_ext = (f, closure) ::
                (List.map2 (fun x v -> (x, v)) formals args) in
            eval (env_ext @ env) body) in
        closure
    | App (f, args) ->
        let f_value = eval env f in
        let arg_values = List.map (eval env) args in
        (match f_value with
        | Closure fn -> fn arg_values
        | _ -> failwith "expected closure")
    | MkArray (e1, e2) -> Array (Array.make (int (eval env e1)) (eval env e2))
    | GetArray (e1, e2) -> (array (eval env e1)).(int (eval env e2))
    | SetArray (e1, e2, e3) ->
        (array (eval env e1)).(int (eval env e2)) <- eval env e3;
        Int 42
    | Seq (e1, e2) -> let _ = eval env e1 in eval env e2
    | Abort -> failwith "abort"
end

let eval = Interp.eval []

let rec free_vars (exp : exp) : IdSet.t = match exp with
| Id x ->
    IdSet.singleton x
| Const _ ->
    IdSet.empty
| Op2 (_, e1, e2) ->
    IdSet.union (free_vars e1) (free_vars e2)
| If (e1, e2, e3) ->
    IdSet.union (free_vars e1) (IdSet.union (free_vars e2) (free_vars e3))
| Let (x, e1, e2) ->
    IdSet.union (free_vars e1) (IdSet.remove x (free_vars e2))
| Fun (f, args, e) ->
    IdSet.diff (free_vars e) (IdSet.of_list (f :: args))
| App (e1, args) ->
    List.fold_left IdSet.union (free_vars e1) (List.map free_vars args)
| MkArray (e1, e2) ->
    IdSet.union (free_vars e1) (free_vars e2)
| GetArray (e1, e2) ->
    IdSet.union (free_vars e1) (free_vars e2)
| SetArray (e1, e2, e3) ->
    IdSet.union (free_vars e1) (IdSet.union (free_vars e2) (free_vars e3))
| Seq (e1, e2) ->
    IdSet.union (free_vars e1) (free_vars e2)
| Abort -> IdSet.empty

let rec subst (x : id) (u : exp) (e : exp) : exp = match (e : exp) with
    | Id y -> if x = y then u else e
    | Const _ -> e
    | Op2 (op, e1, e2) -> Op2 (op, subst x u e1, subst x u e2)
    | If (e1, e2, e3) ->
        If (subst x u e1, subst x u e2, subst x u e3)
    | Let (y, e1, e2) ->
        Let (y, subst x u e1,
            (* TODO(arjun): capture-free *)
             if x = y then e2 else subst x u e2)
    | Fun (f, args, body) ->
        if List.mem x (f :: args) then e
        (* TODO(arjun): capture-free *)
        else Fun (f, args, subst x u body)
    | App (e1, args) -> App (subst x u e1, List.map (subst x u) args)
    | MkArray (e1, e2) -> MkArray (subst x u e1, subst x u e2)
    | GetArray (e1, e2) -> GetArray (subst x u e1, subst x u e2)
    | SetArray (e1, e2, e3) ->
        SetArray (subst x u e1, subst x u e2, subst x u e3)
    | Seq (e1, e2) -> Seq (subst x u e1, subst x u e2)
    | Abort -> Abort
