(** Utilities used by several assignments. *)

(** Source code positions returned by the lexer. *)
type pos = Lexing.position

val string_of_pos : pos -> string

(** Given a function [f] that pretty-prints values to a formatter,
    [make_string_of f] returns a function that pretty-prints values to
    strings. *)
val make_string_of  : (Format.formatter -> 'a -> unit) -> 'a -> string

val parens : bool -> Format.formatter -> (unit -> unit) -> unit