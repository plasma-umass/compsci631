(** An OCaml API for working with SMT-LIB-based solvers, such as Z3. *)

(** {1 Starting solvers.} *)

(** A handle to a Z3 process. *)
type solver

(** [make_solver path] produces a handle to a Z3 process.

  The argument [path] must be the path to the Z3 executable. If [z3] is on the
  [PATH], this can just be ["z3"].

  This command starts Z3 with the flags [-in] and [-smt2]. *)
val make_solver : string -> solver

(** {1 High-level API.}

 This high-level API to Z3 provides simple functions to construct
 terms and send commands to Z3. If Z3 produces an error in response to a
 command, that error is raised as an OCaml exception.
*)


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

type check_sat_result =
  | Sat
  | Unsat
  | Unknown

(** [declare_const solver x sort] runs the command [(declare-const x sort)] *)
val declare_const : solver -> identifier -> sort -> unit

(** [assert_ solver term] runs the command [(assert term)] *)
val assert_ : solver -> term -> unit

(** [check_sat solver] runs the command [(check-sat)] *)
val check_sat : solver -> check_sat_result

(** [push solver] runs the command [(push)] *)
val push : solver -> unit

(** [pop solver] runs the command [(pop)] *)
val pop : solver -> unit

(** The expression [Int] for the solver. *)
val int_sort : sort

(** The expression [Bool] for the solver. *)
val bool_sort : sort

(** [array_sort dom range] produces [(array dom range)] *)
val array_sort : sort -> sort -> sort


val int_to_term : int -> term

val bool_to_term : bool -> term

(** [const x] produces [Const (Id x)], which represents a reference to a
    variable declared with [(declare-const x sort)] *)
val const : string -> term

(** [equals e1 e2] produces [(= e1 e2)] *)
val equals : term -> term -> term

(** [and e1 e2] produces [(and e1 e2)]. In addition, nested [and]s are flattened
    to make debugging easier. *)
val and_ : term -> term -> term

(** [or e1 e2] produces [(or e1 e2)]. In addition, nested [or]s are flattened
    to make debugging easier. *)
val or_ : term -> term -> term

(** [not e] produces [(not e)]. *)
val not : term -> term

(** [implies e1 e2] produces [(=> e1 e2)]. *)
val implies : term -> term -> term

(** [add e1 e2] produces [(+ e1 e2)]. *)
val add : term -> term -> term

(** [sub e1 e2] produces [(- e1 e2)]. *)
val sub : term -> term -> term

(** [mul e1 e2] produces [( * e1 e2)]. *)
val mul : term -> term -> term

(** [lt e1 e2] produces [(< e1 e2)]. *)
val lt : term -> term -> term

(** [> e1 e2] produces [(> e1 e2)]. *)
val gt : term -> term -> term

(** [lte e1 e2] produces [(<= e1 e2)]. *)
val lte : term -> term -> term

(** [gte e1 e2] produces [(>= e1 e2)]. *)
val gte : term -> term -> term

(** {1 Low-level interface} *)

(** The variant of s-expressions used by SMT-LIB. *)
type sexp = Smtlib_syntax.sexp =
  | SList of sexp list
  | SSymbol of string
  | SString of string
  | SKeyword of string
  | SInt of int


(** [command solver sexp] sends a command to the solver and reads a response. *)
val command : solver -> sexp -> sexp

(** [sexp_to_string sexp] returns the s-expressions as a string. *)
val sexp_to_string : sexp -> string
