open Compiler_util

exception Aborted of string

type value =
  | VInt of int
  | VBool of bool
  | VArray of value array
  | VClosure of (value list -> value)
  [@@deriving show]

type env = (id * value) list

let to_int (v : value) = match v with
  | VInt n -> n
  | _ -> failwith "Expected an integer"

let to_bool (v : value) = match v with
  | VBool b -> b
  | _ -> failwith "Expected a boolean"

let to_array (v : value) = match v with
  | VArray arr -> arr
  | _ -> failwith "Expected an array"

let eval'_op (op : op2) (v1 : value) (v2 : value) : value =
  match op with
  | LT -> VBool (to_int v1 < to_int v2)
  | GT -> VBool (to_int v1 > to_int v2)
  | Eq -> VBool (v1 = v2)
  | Add -> VInt (to_int v1 + to_int v2)
  | Sub -> VInt (to_int v1 - to_int v2)
  | Mul -> VInt (to_int v1 * to_int v2)
  | Div ->
     (try VInt (to_int v1 / to_int v2)
      with Division_by_zero -> raise (Aborted "division by zero"))
  | Mod -> VInt (to_int v1 mod to_int v2)

let rec eval' (env : env) (exp : exp) : value = match exp with
  | Fun (f, xs, e) ->
      let rec the_fun (vs : value list) : value =
        assert (List.length vs = List.length xs);
        let env' = (f, VClosure the_fun) :: (List.combine xs vs) @ env in
        eval' env' e in
      VClosure the_fun
  | Id x -> List.assoc x env 
  | Const (Bool b) -> VBool b
  | Const (Int n) -> VInt n
  | Op2 (op, e1, e2) -> eval'_op op (eval' env e1) (eval' env e2)
  | Let (x, e1, e2) -> eval' ((x, eval' env e1) :: env) e2
  | If (e1, e2, e3) ->
     if to_bool (eval' env e1) then eval' env e2 else eval' env e3
  | App (f, es) ->
      let vs = List.map (eval' env) es in
      (match eval' env f with
       | VClosure closure -> closure vs
       | _ -> failwith "Expected closure")
  | MkArray (e1, e2) ->
      VArray (Array.make (to_int (eval' env e1)) (eval' env e2))
  | GetArray (e1, e2) ->
     let arr = to_array (eval' env e1) in
     let ix =  to_int (eval' env e2) in
     (try arr.(ix)
      with Invalid_argument msg -> raise (Aborted msg))
  | SetArray (e1, e2, e3) ->
    let arr = to_array (eval' env e1) in
    let ix = to_int (eval' env e2) in
    let elt = eval' env e3 in
    (try let _ = arr.(ix) <- elt in VInt 0 (* arbitrary *)
     with Invalid_argument msg -> raise (Aborted msg))
  | Seq (e1, e2) ->
      let _ = eval' env e1 in
      eval' env e2
  | Abort -> raise (Aborted "Abort expression encountered")

 let eval e = eval' [] e
