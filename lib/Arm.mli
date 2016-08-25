(** A simple interface to generate ARM assembly.
 *
 * Thanks to Sam Baxter for developing an early version of this module.
 *
 * This module lets you generate basic ARM assembly, but provides some 
 * convenient features. For example, although [add r0, r1, #42] is a valid
 * ARM instruction, [add r0, #12, #42] is not. However, [mov r0, #54] has
 * the desired effect. This module lets you write [add x y z] and generates
 * either [add] or [mov] as appropriate. It has similar conveniences for
 * other arithmetic and comparison operations.
 *)

type assembly

type index

type label = string

type reg =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | SP (** a.k.a. r13 *)
  | LR (** a.k.a. r14 *)
  | PC (** a.k.a. r15 *)

type operand =
  | Imm of int
  | Reg of reg

type cond = EQ | NE | GE | GT | LE | LT

val ix : operand -> index
val ix_lsl : operand -> int -> index

val noop : assembly

val seq : assembly -> assembly -> assembly

val concat : assembly list -> assembly

val comment : string -> assembly

val label : string -> assembly

val mov : ?cond:cond -> reg -> operand -> assembly

val ldr : ?cond:cond -> ?index:index -> reg -> reg -> assembly

val str : ?cond:cond -> ?index:index -> operand -> reg -> assembly

val cmp : ?cond:cond -> operand -> operand -> assembly

val add : ?cond:cond -> reg -> operand -> operand -> assembly

val sub : ?cond:cond -> reg -> operand -> operand -> assembly

val add : ?cond:cond -> reg -> operand -> operand -> assembly

val mul : ?cond:cond -> reg -> operand -> operand -> assembly

val adr : ?cond:cond -> reg -> label -> assembly

val push : operand -> assembly

val pop : reg -> assembly

val bl : ?cond:cond -> label -> assembly

val b : ?cond:cond -> label -> assembly

val bx : ?cond:cond -> reg -> assembly

(** Maps string ["R0"] to [R0], etc. *)
val string_to_reg : string -> reg

(** Produces 0 for R0, 1 for R1, ... *)
val reg_index : reg -> int
                                
val string_of_assembly : assembly -> string
