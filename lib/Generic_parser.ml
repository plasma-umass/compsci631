open MParser
open MParser_RE
open Tokens

type 'a parser = ('a, unit) MParser.t
         
let app_pattern p mk_app = 
  let rec f e1 s = (
    (p >>= fun e1' -> f (mk_app e1 e1')) <|>
        (return e1)) s in
  p >>= f

(* Keywords that could be confused for identifiers. *)
let reserved_words = [
    "true"; "false"; "empty"; "head"; "tail"; "empty?"; "if"; "then";
    "else"; "let"; "in"; "fun"; "fix"; "bool"; "int"; "list"; "array";
    "tfun"; "is_empty"
  ]

let id : string parser =
  attempt (
      regexp (make_regexp "[A-Za-z_][A-Za-z_0-9_']*") >>= fun str ->
      if List.mem str reserved_words then fail "reserved word" else return str)
  <<< spaces


let rev_fold_left f xs = match List.rev xs with
  | [] -> raise (Failure "expected at least one element (internal error)")
  | x :: xs -> List.fold_left f x xs

let from_string exp (str : string) = match parse_string (spaces >> exp) str () with
  | Success exp -> exp
  | Failed (msg, _) ->
    Printf.eprintf "%s\n%!" msg; failwith msg

let from_file exp (fname : string) =
  let chan = open_in fname in
  match parse_channel (spaces >> exp) chan () with
  | Success exp -> exp
  | Failed (msg, _) ->
    Printf.eprintf "%s\n%!" msg;
    failwith msg
