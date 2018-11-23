type reg = int

type value = Reg of reg | Imm of int

type op2 = Add | Sub | Mul | Div | Mod | LT | Eq

type instr =
    | Goto of value
    | Exit of value
    | Abort
    | Op2 of reg * op2 * value * value * instr
    | Copy of reg * value * instr
    | Load of reg * value * instr
    | Store of reg * value * instr
    | Ifz of reg * instr * instr
    | Malloc of reg * value * instr
    | Free of reg * instr

type block = int * instr

val string_of_blocks : block list -> string

