include Imp_syntax

open PL_util

module Parsers = Lexparse_util.MakeParsers (struct
    exception ParseError = Parsing.Parse_error
    type token = Imp_parser.token
    type exp = bexp * cmd * bexp
    let parser = Imp_parser.program
    let lexer = Imp_lexer.token
  end)

let from_file = Parsers.from_file
let from_string = Parsers.from_string

module Format = struct

  open Format

  type cxt = NOT | ATOM | MUL | ADD | CMP | AND | OR | EXP

  let print_aparen (cxt : cxt) (e : aexp) : bool = match e with
    | AConst _ -> false
    | AVar _ -> false
    | AOp (Add, _, _) -> cxt < ADD
    | AOp (Sub, _, _) -> cxt < ADD
    | AOp (Mul, _, _) -> cxt < MUL

  let print_bparen (cxt : cxt) (e : bexp) : bool = match e with
    | BConst _ -> false
    | BAnd _ -> cxt < AND
    | BOr _ -> cxt < OR
    | BNot _ -> cxt < NOT
    | BCmp _ -> cxt < CMP

  let rec aexp (cxt : cxt) (fmt : formatter) (e : aexp) : unit =
    parens (print_aparen cxt e) fmt (fun () ->
        match e with
        | AConst n -> fprintf fmt "@[%d@]" n
        | AVar x -> fprintf fmt "@[%s@]" x
        | AOp (Add, e1, e2) ->
          fprintf fmt "@[%a +@ %a@]" (aexp ADD) e1 (aexp MUL) e2
        | AOp (Sub, e1, e2) ->
          fprintf fmt "@[%a -@ %a@]" (aexp ADD) e1 (aexp MUL) e2
        | AOp (Mul, e1, e2) ->
          fprintf fmt "@[%a *@ %a@]" (aexp MUL) e1 (aexp ATOM) e2)

  let rec bexp (cxt : cxt) (fmt : formatter) (e : bexp) : unit =
    parens (print_bparen cxt e) fmt (fun () ->
        match e with
        | BConst n -> fprintf fmt "@[%b@]" n
        | BNot e -> fprintf fmt "@[!%a@]" (bexp NOT) e
        | BAnd (e1, e2) ->
          fprintf fmt "@[%a &&@ %a@]" (bexp AND) e1 (bexp ATOM) e2
        | BOr (e1, e2) ->
          fprintf fmt "@[%a ||@ %a@]" (bexp OR) e1 (bexp AND) e2
        | BCmp (Lt, e1, e2) ->
          fprintf fmt "@[%a <@ %a@]" (aexp CMP) e1 (aexp CMP) e2
        | BCmp (Eq, e1, e2) ->
          fprintf fmt "@[%a ==@ %a@]" (aexp CMP) e1 (aexp CMP) e2
        | BCmp (Gt, e1, e2) ->
          fprintf fmt "@[%a >@ %a@]" (aexp CMP) e1 (aexp CMP) e2
        | BCmp (Lte, e1, e2) ->
          fprintf fmt "@[%a <=@ %a@]" (aexp CMP) e1 (aexp CMP) e2
        | BCmp (Gte, e1, e2) ->
          fprintf fmt "@[%a >=@ %a@]" (aexp CMP) e1 (aexp CMP) e2)

end

let bexp_to_string = make_string_of (Format.bexp Format.EXP)