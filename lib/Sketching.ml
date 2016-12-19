type op2 =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BAnd
  | BOr
  | LAnd
  | LOr
  | Eq

type op1 = LNot | BNot

type id = string

type exp =
  | EId of id
  | EInt of int
  | EWidth
  | EOp2 of op2 * exp * exp
  | EOp1 of op1 * exp
  | EHole of int

type cmd =
  | CSkip
  | CAbort
  | CAssign of id * exp
  | CIf of exp * cmd * cmd
  | CSeq of cmd * cmd
  | CRepeat of id * exp * cmd

module Parser = struct
  open MParser
  open MParser_RE
  open Tokens

  type 'a parser = ('a, unit) MParser.t

  let infix sym op : (exp, unit) operator =
    let f e1 e2 = EOp2 (op, e1, e2) in
    Infix  (skip_symbol sym >> return f, Assoc_left)

  let operators : (exp, unit) operator list list = [
    [infix "*" Mul; infix "/" Div; infix "%" Mod];
    [infix "+" Add; infix "-" Sub];
    [infix "<<" LShift; infix ">>" RShift];
    [infix "&" BAnd];
    [infix "|" BOr];
    [infix "==" Eq];
    [infix "and" LAnd];
    [infix "or" LOr];

  ]

  let id = regexp (make_regexp "[A-Za-z_][A-Za-z_0-9_']*") <<< spaces

  let next_hole : unit -> int =
    let i = ref 0 in
    fun () ->
      let x = !i in
      i := x + 1;
      x

  let rec term s  =
    (parens expr <|>
     (decimal |>> (fun n -> EInt n)) <|>
     (symbol "W" |>> (fun _ -> EWidth)) <|>
     (symbol "~" >>= fun _ ->
      term >>= fun e ->
      return (EOp1 (BNot, e))) <|>
     (symbol "not" >>= fun _ ->
      expr >>= fun e ->
      return (EOp1 (LNot, e))) <|>
     (symbol "??" |>> (fun _ -> EHole (next_hole ()))) <|>
     (id |>> (fun x -> EId x))
     ) s

  and expr s = expression operators term s

  let rec acmd s =
    (braces cmd <|>
     (symbol "skip" >>= fun _ -> return CSkip) <|>
     (symbol "abort" >>= fun _ -> return CAbort) <|>
     if_ <|>
     repeat_ <|>
     assign) s

  and if_ s =
    (symbol "if" >>= fun _ ->
     parens expr >>= fun pred ->
     acmd >>= fun cmd1 ->
     (symbol "else" >>= fun _ ->
      acmd >>= fun cmd2 ->
      return (CIf (pred, cmd1, cmd2))) <|>
     (return (CIf (pred, cmd1, CSkip)))) s

  and repeat_ s =
    (symbol "repeat" >>= fun _ ->
     id >>= fun x ->
     symbol ":" >>= fun _ ->
     expr >>= fun bound ->
     acmd >>= fun cmd ->
     return (CRepeat (x, bound, cmd))) s

  and assign s =
    (id >>= fun x ->
     symbol "=" >>= fun _ ->
     expr >>= fun e ->
     symbol ";" >>= fun _ ->
     return (CAssign (x, e))) s

  and cmd s =
    (acmd >>= fun c1 ->
     ((cmd |>> fun c2 -> CSeq (c1, c2)) <|>
      return c1)) s

  let from_string = Generic_parser.from_string cmd

  let from_file = Generic_parser.from_file cmd

end

let from_string = Parser.from_string

let from_file = Parser.from_file

module Format = struct

  open Format

  type cxt = NOT | MUL | ADD | SHIFT | BAND | BOR | AND | OR | EQ

  let print_paren (cxt : cxt) (exp : exp) : bool = match exp with
    | EOp2 (op, _, _) -> (match op with
      | Mul | Div | Mod -> cxt < MUL
      | Add | Sub -> cxt < ADD
      | LShift | RShift -> cxt < SHIFT
      | BAnd  -> cxt < BAND
      | BOr -> cxt < BOR
      | Eq -> cxt < EQ
      | LAnd -> cxt < AND
      | LOr -> cxt < OR)
    | _ -> false

  let rec exp (cxt : cxt) (fmt : formatter) (e : exp) : unit =
    PL_util.parens (print_paren cxt e) fmt (fun () ->
        match e with
        | EInt n -> fprintf fmt "@[%d@]" n
        | EWidth -> fprintf fmt "W"
        | EId x -> fprintf fmt "@[%s@]" x
        | EHole _ -> fprintf fmt "??"
        | EOp1 (LNot, e) -> fprintf fmt "@[not %a@]" (exp NOT) e
        | EOp1 (BNot, e) -> fprintf fmt "@[~%a@]" (exp NOT) e
        | EOp2 (Mul, e1, e2) ->
          fprintf fmt "@[%a *@ %a@]" (exp MUL) e1 (exp MUL) e2
        | EOp2 (Div, e1, e2) ->
          fprintf fmt "@[%a /@ %a@]" (exp MUL) e1 (exp MUL) e2
        | EOp2 (Mod, e1, e2) ->
          fprintf fmt "@[%a %@ %a@]" (exp MUL) e1 (exp MUL) e2
        | EOp2 (Add, e1, e2) ->
          fprintf fmt "@[%a +@ %a@]" (exp ADD) e1 (exp ADD) e2
        | EOp2 (Sub, e1, e2) ->
          fprintf fmt "@[%a -@ %a@]" (exp ADD) e1 (exp ADD) e2
        | EOp2 (LShift, e1, e2) ->
          fprintf fmt "@[%a <<@ %a@]" (exp SHIFT) e1 (exp SHIFT) e2
        | EOp2 (RShift, e1, e2) ->
          fprintf fmt "@[%a >>@ %a@]" (exp SHIFT) e1 (exp SHIFT) e2
        | EOp2 (BAnd, e1, e2) ->
          fprintf fmt "@[%a &@ %a@]" (exp BAND) e1 (exp BAND) e2
        | EOp2 (BOr, e1, e2) ->
          fprintf fmt "@[%a |@ %a@]" (exp BOR) e1 (exp BOR) e2
        | EOp2 (LAnd, e1, e2) ->
          fprintf fmt "@[%a and@ %a@]" (exp AND) e1 (exp AND) e2
        | EOp2 (LOr, e1, e2) ->
          fprintf fmt "@[%a or@ %a@]" (exp OR) e1 (exp OR) e2
        | EOp2 (Eq, e1, e2) ->
          fprintf fmt "@[%a ==@ %a@]" (exp EQ) e1 (exp EQ) e2)

  let rec cmd (fmt : formatter) (c : cmd) : unit =
    match c with
    | CAssign (x, e) -> fprintf fmt "@[%s =@ %a;@]" x (exp EQ) e
    | CIf (e, c1, c2) ->
      fprintf fmt "@[if (@[%a@])@ { %a } else { %a }@]" (exp EQ) e cmd c1 cmd c2
    | CSeq (c1, c2) -> fprintf fmt "%a@ %a" cmd c1 cmd c2
    | CSkip -> ()
    | CAbort -> fprintf fmt "abort;"
    | CRepeat (x, e, c) -> fprintf fmt "repeat %s : %a %a" x (exp EQ) e cmd c

end

let to_string = PL_util.make_string_of Format.cmd
