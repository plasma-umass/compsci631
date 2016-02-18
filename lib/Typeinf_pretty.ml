open PL_util
open Typeinf_syntax
open Explicit

module FormatTyp = struct

  open Format

  type cxt = ATOM | FUN | TYPS

  let print_paren (cxt : cxt) (t : typ) : bool = match t with
    | TMetavar _ -> false
    | TInt -> false
    | TBool -> false
    | TFun _ -> cxt < FUN
    | TPair _ -> cxt < TYPS
    | TList _ -> false

  let rec typ (cxt : cxt) (fmt : formatter) (t : typ) : unit =
    parens (print_paren cxt t) fmt (fun () ->
      match t with
        | TMetavar (n, x) -> fprintf fmt "%s-%d" x n
        | TInt -> fprintf fmt "int"
        | TBool -> fprintf fmt "bool"
        | TFun (t1, t2) ->
          fprintf fmt "@[<hv 2>%a@ -> %a@]" (typ ATOM) t1 (typ FUN) t2
        | TPair (t1, t2) ->
          fprintf fmt "@[<hv 2>%a,@ %a@]" (typ TYPS) t1 (typ FUN) t2
        | TList t ->
          fprintf fmt "@[%a@ list@]" (typ ATOM) t)

end

let format_typ = FormatTyp.typ FormatTyp.FUN


module Format = struct

  open Format

  type cxt = ATOM | APP | LIST | MUL | ADD | CMP | EXP | EXPS

  let print_paren (cxt : cxt) (e : exp) : bool = match e with
    | Const _ -> false
    | Op2 (Add, _, _) -> cxt < ADD
    | Op2 (Sub, _, _)-> cxt < ADD
    | Op2 (Mul, _, _) -> cxt < MUL
    | Op2 (_, _, _) -> cxt < CMP
    | If _ -> cxt < EXP
    | Id _ -> false
    | Let _ -> cxt < EXP
    | Fun _ -> cxt < EXP
    | Fix _ -> cxt < EXP
    | App _ -> cxt < APP
    | Empty _ -> false
    | Cons _ -> cxt < LIST
    | Head _ -> false
    | Tail _ -> false
    | IsEmpty _ -> false
    | Pair _ -> cxt < EXPS
    | ProjL _ -> false
    | ProjR _ -> false

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit =
    parens (print_paren cxt e) fmt (fun () ->
        match e with
        | Const (Int n) -> fprintf fmt "@[%d@]" n
        | Const (Bool true) -> fprintf fmt "true"
        | Const (Bool false) -> fprintf fmt "false"
        | Op2 (Add, e1, e2) ->
          fprintf fmt "@[%a +@ %a@]" (exp ADD) e1 (exp MUL) e2
        | Op2 (Sub, e1, e2) ->
          fprintf fmt "@[%a -@ %a@]" (exp ADD) e1 (exp MUL) e2
        | Op2 (Mul, e1, e2) ->
          fprintf fmt "@[%a *@ %a@]" (exp MUL) e1 (exp LIST) e2
        | Op2 (LT, e1, e2) -> fprintf fmt "@[%a <@ %a@]" (exp ADD) e1 (exp ADD) e2
        | Op2 (GT, e1, e2) -> fprintf fmt "@[%a >@ %a@]" (exp ADD) e1 (exp ADD) e2
        | Op2 (Eq, e1, e2) -> fprintf fmt "@[%a =@ %a@]" (exp ADD) e1 (exp ADD) e2
        | If (e1, e2, e3) ->
          fprintf fmt "@[if %a@;<1 2>@[then@;<1 2>%a@]@;<1 2>@[else@;<1 2>%a@]@]"
            (exp EXP) e1 (exp EXP) e2 (exp EXP) e3
        | Id x -> fprintf fmt "@[%s@]" x
        | Let (x, e1, e2) ->
          fprintf fmt "@[<v>let @[%s =@;<1 2>%a in@]@ %a@]"
            x (exp EXP) e1 (exp EXP) e2
        | Fun (x, t, e) ->
          fprintf fmt "@[<hv 2>fun (%s : %a) ->@ %a" x
            format_typ t (exp EXP) e
        | Fix (x, t, e) ->
          fprintf fmt "@[<hv 2>fix (%s : %a) ->@ %a" x
            format_typ t (exp EXP) e
        | App (e1, e2) ->
          fprintf fmt "@[%a@ %a@]" (exp APP) e1 (exp ATOM) e2
        | Empty t -> fprintf fmt "@[empty<%a>@]" format_typ t
        | Cons (e1, e2) ->
          fprintf fmt "@[<hv>%a@ ::@ %a@]" (exp ATOM) e1 (exp LIST) e2
        | IsEmpty e -> fprintf fmt "@[<hv 2>empty?@ %a@]" (exp ATOM) e
        | Head e -> fprintf fmt "@[<hv 2>head@ %a@]" (exp ATOM) e
        | Tail e -> fprintf fmt "@[<hv 2>tail@ %a@]" (exp ATOM) e
        | Pair (e1, e2) ->
          fprintf fmt "@[(@[<hv 2>%a,@ %a@])@]" (exp EXPS) e1 (exp EXP) e2
        | ProjL e ->
          fprintf fmt "@[%a.1@]" (exp ATOM) e
        | ProjR e ->
          fprintf fmt "@[%a.2@]" (exp ATOM) e)

end

let format_exp = Format.exp Format.EXP

let string_of_exp (e : exp) : string = make_string_of format_exp e

let print_exp (e : exp) : unit = print_string (string_of_exp e)

let string_of_typ (t : typ) : string = make_string_of format_typ t

let print_typ (t : typ) : unit = print_string (string_of_typ t)