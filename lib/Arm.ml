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

type index =
  | Imm of int
  | Reg of reg
  | LSL of reg * int

type operation =
  | Mov
  | Ldr
  | Adr
  | Str
  | Add
  | B
  | BL
  | BX
  | Cmp
  | Cmn
  | Sub
  | Rsb
  | Mul

type operand =
  | Imm of int
  | Reg of reg

type cond = EQ | NE | GE | GT | LE | LT

type full_operand =
  | Op of operand
  | Addr of reg
  | Index of reg * index
  | Label of label
      
type instr = {
    cond: cond option;
    operation: operation;
    operands : full_operand list
  }

type pseudo_instr =
  | Instr of instr
  | Push of operand
  | Pop of reg
  | Label of label
  | Comment of string


type assembly = pseudo_instr list

let instr op cond dst src operand: assembly =
  match operand with
  | None -> [ Instr { cond = cond; operation=op; operands = [ Op (Reg dst); src ] } ]
  | Some src' -> [ Instr { cond = cond; operation=op; operands = [ Op (Reg dst); src; Op (src') ] } ]

let ix (op : operand) : index = match op with
  | Imm n -> Imm n
  | Reg r -> Reg r

let ix_lsl (op : operand) (shift : int) : index = match op with
  | Reg r -> LSL (r, shift)
  | Imm n -> Imm (n lsl shift)

let noop : assembly = instr Mov None R0 (Op (Reg R0)) None

let seq (a1 : assembly) (a2 : assembly) : assembly = a1 @ a2

let rec concat (alist : assembly list) : assembly = match alist with
  | [] -> noop
  | [a] -> a
  | a :: rest -> seq a (concat rest)

let comment (s : string) : assembly = [ Comment s ]

let label (s : string) : assembly = [ Label s ]

let mov ?(cond : cond option) (dst : reg) (src: operand) : assembly =
  instr Mov cond dst (Op src) None

let ldr ?(cond : cond option) ?(index : index option) (dst : reg) (src : reg) : assembly =
  instr Ldr cond dst (match index with
                      | None -> Addr src
                      | Some offset -> Index (src, offset))
        None

let str ?(cond : cond option) ?(index : index option) (src : operand) (dst : reg) : assembly =
  let dst' = match index with
    | None -> Addr dst
    | Some offset -> Index (dst, offset) in
  [ Instr { cond = cond; operation = Str; operands = [ Op src; dst' ] } ]

let add ?(cond : cond option) (dst : reg) (src1 : operand) (src2 : operand) : assembly = match (src1, src2) with
  | (Imm x, Imm y) -> instr Mov cond dst (Op (Imm (x + y))) None
  | (Reg _, _) -> instr Add cond dst (Op src1) (Some src2)
  | (Imm _, Reg _) -> instr Add cond dst (Op src2) (Some src1)

let sub ?(cond : cond option) (dst : reg) (src1 : operand) (src2 : operand) : assembly = match (src1, src2) with
  | (Imm x, Imm y) -> instr Mov cond dst (Op (Imm (x - y))) None
  | (Reg _, _) -> instr Sub cond dst (Op src1) (Some src2)
  | (Imm _, Reg _) -> instr Rsb cond dst (Op src2) (Some src1)

let mul ?(cond : cond option) (dst : reg) (src1 : operand) (src2 : operand) : assembly = match (src1, src2) with
  | (Imm x, Imm y) -> instr Mov cond dst (Op (Imm (x * y))) None
  | (Reg _, _) -> instr Mul cond dst (Op src1) (Some src2)
  | (Imm _, Reg _) -> instr Mul cond dst (Op src2) (Some src1)

let cmp ?(cond : cond option) (src1 : operand) (src2 : operand) : assembly = match (src1, src2) with
  | (Imm x, Imm y) ->
     if x = y then instr Cmp cond R0 (Op (Reg R0)) None (* Compare whatever is in R0 with itself *)
     else instr Cmn cond R0 (Op (Reg R0)) None
  | (Reg r, _) -> instr Cmp cond r (Op src2) None
  | (Imm _, Reg r) -> instr Cmn cond r (Op src1) None

let adr ?(cond : cond option) (dst : reg) (l : label) : assembly = instr Adr cond dst (Label l) None

let push (src : operand) : assembly = [Push src]

let pop (dst : reg) : assembly = [Pop dst]

let bl ?(cond : cond option) (l : label) : assembly = [ Instr { cond = cond; operation=BL; operands = [ Label l ] } ]

let b ?(cond : cond option) (l : label) : assembly = [ Instr { cond = cond; operation=B; operands = [ Label l ] } ]

let bx ?(cond : cond option) (r : reg) : assembly = [ Instr { cond = cond; operation=BX; operands = [ Op (Reg r) ] } ]

let string_to_reg (s : string) : reg = match s with
  | "R0" -> R0
  | "R1" -> R1
  | "R2" -> R2
  | "R3" -> R3
  | "R4" -> R4
  | "R5" -> R5
  | "R6" -> R6
  | "R7" -> R7
  | "R8" -> R8
  | "R9" -> R9
  | "R10" -> R10
  | "R11" -> R11
  | "R12" -> R12
  | "SP" -> SP 
  | "LR" -> LR
  | "PC" -> PC
  | _ -> raise (Invalid_argument ("string_to_reg: invalid register name: " ^ s))

let reg_index r = match r with
  | R0 -> 0
  | R1 -> 1
  | R2 -> 2
  | R3 -> 3
  | R4 -> 4
  | R5 -> 5
  | R6 -> 6
  | R7 -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | SP -> 13
  | LR -> 14
  | PC -> 15

module Printer = struct
    open Format
    
    let of_reg r = match r with
      | R0 -> "r0"
      | R1 -> "r1"
      | R2 -> "r2"
      | R3 -> "r3"
      | R4 -> "r4"
      | R5 -> "r5"
      | R6 -> "r6"
      | R7 -> "r7"
      | R8 -> "r8"
      | R9 -> "r9"
      | R10 -> "r10"
      | R11 -> "r11"
      | R12 -> "r12"
      | SP -> "sp"
      | LR -> "lr"
      | PC -> "pc"

    let of_operation (operation : operation) = match operation with
      | Mov -> "mov"
      | Ldr -> "ldr"
      | Adr -> "adr"
      | Str -> "str"
      | Add -> "add"
      | B -> "b"
      | BL -> "bl"
      | BX -> "bx"
      | Cmp -> "cmp"
      | Cmn -> "cmn"
      | Sub -> "sub"
      | Rsb -> "rsb"
      | Mul -> "mul"

    let of_cond (cond : cond) = match cond with
      | EQ -> "eq"
      | NE -> "ne"
      | GE -> "ge"
      | GT -> "gt"
      | LE -> "le"
      | LT -> "lt"

    let of_cond_opt (opt_cond : cond option) = match opt_cond with
      | None -> ""
      | Some c -> of_cond c

    let of_operand (fmt : formatter) (operand : operand) = match operand with
      | Imm n -> fprintf fmt "#%d" n
      | Reg r -> fprintf fmt "%s" (of_reg r)

    let of_index (fmt : formatter) (index : index) = match index with
      | Imm n -> fprintf fmt "#%d" n
      | Reg r -> fprintf fmt "%s" (of_reg r)
      | LSL (r, n) -> fprintf fmt "+%s, LSL #%d" (of_reg r) n

    let of_full_operand (fmt : formatter) (x : full_operand) = match x with
      | Op op -> of_operand fmt op
      | Addr reg -> fprintf fmt "[%s]" (of_reg reg)
      | Index (reg, index) -> fprintf fmt "[%s, +%a]" (of_reg reg) of_index index
      | Label s -> fprintf fmt "%s" s

    let of_instr (fmt : formatter) (x : instr) =
      let pp_comma fmt () = pp_print_string fmt "," in
      let pp_operands =  pp_print_list ~pp_sep:pp_comma of_full_operand in
      fprintf fmt "%s%s %a" (of_operation x.operation) (of_cond_opt x.cond) pp_operands x.operands

    let of_pseudo_instr (fmt : formatter) (x : pseudo_instr) = match x with
      | Instr i -> of_instr fmt i
      | Push src -> fprintf fmt "push { %a }" of_operand src
      | Pop dst -> fprintf fmt "pop { %s }" (of_reg dst)
      | Label l -> fprintf fmt "%s:" l
      | Comment c -> fprintf fmt "/* %s */" c

    let of_assembly (fmt : formatter) (x : assembly) : unit =
      pp_open_vbox fmt 0;
      pp_print_list ~pp_sep:pp_force_newline of_pseudo_instr fmt x;
      pp_close_box fmt ()

  end

let string_of_assembly (asm : assembly) : string = 
  let open Format in
  let buf = Buffer.create 500 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  Printer.of_assembly fmt asm;
  Buffer.contents buf
