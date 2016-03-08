include Smtlib_syntax

type solver = { stdin : out_channel; stdout_lexbuf : Lexing.lexbuf }

(* Does not flush *)
let rec write_sexp (out_chan : out_channel) (e : sexp): unit = match e with
  | SInt n -> output_string out_chan (string_of_int n)
  | SSymbol str -> output_string out_chan str
  | SKeyword str -> output_string out_chan str
  | SString str ->
    (output_char out_chan '(';
     output_string out_chan str;
     output_char out_chan ')')
  | SList lst ->
    (output_char out_chan '(';
     write_sexp_list out_chan lst;
     output_char out_chan ')')

and write_sexp_list (out_chan : out_channel) (es : sexp list) : unit =
  match es with
    | [] -> ()
    | [e] -> write_sexp out_chan e
    | e :: es ->
      (write_sexp out_chan e;
       output_char out_chan ' ';
       write_sexp_list out_chan es)

let write (solver : solver) (e : sexp) : unit =
  write_sexp solver.stdin e;
  output_char solver.stdin '\n';
  flush solver.stdin

let read (solver : solver) : sexp =
  Smtlib_parser.sexp Smtlib_lexer.token solver.stdout_lexbuf

let command (solver : solver) (sexp : sexp) = write solver sexp; read solver

let print_success_command =
  SList [SSymbol "set-option"; SKeyword ":print-success"; SSymbol "true"]

let make_solver (z3_path : string) : solver =
  let open Unix in
  let (z3_stdin, z3_stdin_writer) = pipe () in
  let (z3_stdout_reader, z3_stdout) = pipe () in
  let _ = create_process z3_path [| z3_path; "-in"; "-smt2" |]
    z3_stdin z3_stdout stderr in
  let in_chan = in_channel_of_descr z3_stdout_reader in
  let out_chan = out_channel_of_descr z3_stdin_writer in
  set_binary_mode_out out_chan false;
  set_binary_mode_in in_chan false;
  let solver = { stdin = out_chan; stdout_lexbuf = Lexing.from_channel in_chan } in
  match command solver print_success_command with
    | SSymbol "success" -> solver
    | _ -> failwith "could not configure solver to :print-success"

let sexp_to_string (sexp : sexp) : string =
  let open Buffer in
  let buf = create 100 in
  let rec to_string (sexp : sexp) : unit = match sexp with
    | SList alist -> add_char buf '('; list_to_string alist; add_char buf ')'
    | SSymbol x -> add_string buf x;
    | SKeyword x -> add_string buf x;
    | SString x -> add_char buf '"'; add_string buf x; add_char buf '"'
    | SInt n -> add_string buf (string_of_int n)
  and list_to_string (alist : sexp list) : unit = match alist with
    | [] -> ()
    | [x] -> to_string x
    | x :: xs -> to_string x; add_char buf ' '; list_to_string xs in
  to_string sexp;
  contents buf

type check_sat_result =
  | Sat
  | Unsat
  | Unknown

type identifier =
  | Id of string

type sort =
  | Sort of identifier
  | SortApp of identifier * sort list

type term =
  | String of string
  | Int of int
  | Const of identifier
  | App of identifier * term list
  | Let of string * term * term

let id_to_sexp (id : identifier) : sexp = match id with
  | Id x -> SSymbol x

let rec sort_to_sexp (sort : sort) : sexp = match sort with
  | Sort x -> id_to_sexp x
  | SortApp (x, sorts) ->
    SList ((id_to_sexp x) :: (List.map sort_to_sexp sorts))

let rec term_to_sexp (term : term) : sexp = match term with
  | String s -> SString s
  | Int n -> SInt n
  | Const x -> id_to_sexp x
  | App (f, args) -> SList (id_to_sexp f :: (List.map term_to_sexp args))
  | Let (x, term1, term2) ->
    SList [SSymbol "let";
           SList [SList [SSymbol x; term_to_sexp term1]];
           term_to_sexp term2]

let expect_success (solver : solver) (sexp : sexp) : unit =
  match command solver sexp with
  | SSymbol "success" -> ()
  | SList [SSymbol "error"; SString x] -> failwith x
  | sexp -> failwith ("expected either success or error from solver, got " ^
                      (sexp_to_string sexp))

let declare_const (solver : solver) (id : identifier) (sort : sort) : unit =
  expect_success solver
    (SList [SSymbol "declare-const"; id_to_sexp id; sort_to_sexp sort])

let assert_ (solver : solver) (term : term) : unit =
  expect_success solver (SList [SSymbol "assert"; term_to_sexp term])

let check_sat (solver : solver) : check_sat_result =
  match command solver (SList [SSymbol "check-sat"]) with
  | SSymbol "sat" -> Sat
  | SSymbol "unsat" -> Unsat
  | SSymbol "unknown" -> Unknown
  | sexp -> failwith ("unexpected result from (check-sat), got " ^
                      sexp_to_string sexp)

let push (solver : solver) = expect_success solver (SList [SSymbol "push"])
let pop (solver : solver) = expect_success solver (SList [SSymbol "pop"])

let int_sort  = Sort (Id "Int")

let bool_sort = Sort (Id "Bool")

let array_sort dom rng = SortApp (Id "Array", [dom; rng])

let int_to_term n = Int n

let bool_to_term b = match b with
  | true -> Const (Id "true")
  | false -> Const (Id "false")

let app2 x term1 term2 = App (Id x, [term1; term2])

let equals = app2 "="

let and_ = app2 "and"

let or_ = app2 "or"

let not term = App (Id "not", [term])

let implies = app2 "=>"

let add = app2 "+"

let mul = app2 "*"

let lt = app2 "<"
