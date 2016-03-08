(** An OCaml API for working with SMT-LIB-based solvers, such as Z3. *)

(** The variant of s-expressions used by SMT-LIB. *)
type sexp = Smtlib_syntax.sexp =
  | SList of sexp list
  | SSymbol of string
  | SString of string
  | SKeyword of string
  | SInt of int

(** A handle to a Z3 process. *)
type solver

(** [make_solver path] produces a handle to a Z3 process.

  The argument [path] must be the path to the Z3 executable. If [z3] is on the
  [PATH], this can just be ["z3"]. *)
val make_solver : string -> solver

(** [command solver sexp] sends a command to the solver and reads a response. *)
val command : solver -> sexp -> sexp

val sexp_to_string : sexp -> string

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

val declare_const : solver -> identifier -> sort -> unit
val assert_ : solver -> term -> unit
val check_sat : solver -> check_sat_result
val push : solver -> unit
val pop : solver -> unit

val int_sort : sort
val bool_sort : sort
val array_sort : sort -> sort -> sort

val int_to_term : int -> term
val bool_to_term : bool -> term
val equals : term -> term -> term
val and_ : term -> term -> term
val or_ : term -> term -> term
val not : term -> term
val implies : term -> term -> term
val add : term -> term -> term
val mul : term -> term -> term
val lt : term -> term -> term
