type pos = Lexing.position
type formatter = Format.formatter

let string_of_pos pos = 
  let open Lexing in
  if String.length pos.pos_fname > 0 then
    Format.sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
  else
    Format.sprintf "line %d, column %d" pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

let make_string_of formatter x =
  let open Format in
  let buf = Buffer.create 100 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  formatter fmt x;
  fprintf fmt "@?";
  Buffer.contents buf

let parens (on : bool) (fmt : formatter) (thunk : unit -> unit) : unit =
  match on with
  | false -> thunk ()
  | true ->
    let open Format in
    pp_open_box fmt 0;
    pp_print_string fmt "(";
    thunk ();
    pp_print_string fmt ")";
    pp_close_box fmt ()
