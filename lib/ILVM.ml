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

module Printer = struct
    open Format

    let of_op2 (op2 : op2) : string = match op2 with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "%"
        | LT -> "<"
        | Eq -> "=="

    let of_value (v : value) : string = match v with
        | Reg n -> sprintf "r%d" n
        | Imm n -> sprintf "%d" n

    let rec of_instr (fmt : formatter) (instr : instr) : unit = match instr with
        | Goto v -> fprintf fmt "goto(%s);" (of_value v)
        | Exit v -> fprintf fmt "exit(%s);" (of_value v)
        | Abort -> fprintf fmt "abort;"
        | Op2 (x, op, v1, v2, rest) ->
            fprintf fmt "r%d = %s %s %s;\n%a" x (of_value v1) (of_op2 op)
                (of_value v2) of_instr rest;
        | Copy (x, v, rest) ->
            fprintf fmt "r%d = %s;\n%a" x (of_value v) of_instr rest;
        | Load (x, v, rest) ->
            fprintf fmt "r%d = *%s;\n%a" x (of_value v) of_instr rest;
        | Store (x, v, rest) ->
            fprintf fmt "*r%d = %s;\n%a" x (of_value v) of_instr rest;
        | Ifz (x, rest1, rest2) ->
            fprintf fmt "ifz r%d {\n@[%a@]\n} else {\n@[%a@]\n}" x
                of_instr rest1 of_instr rest2;
        | Malloc (x, v, rest) ->
            fprintf fmt "r%d = malloc(%s);\n%a" x (of_value v) of_instr rest;
        | Free (x, rest) ->
            fprintf fmt "free(r%d);\n%a" x of_instr rest

    let of_block (fmt : formatter) ((n, instr) : block) : unit =
        fprintf fmt "block %d {\n@[%a@]\n}" n of_instr instr

    let of_blocks (fmt : formatter) (blocks : block list) : unit =
      pp_open_vbox fmt 0;
      pp_print_list ~pp_sep:pp_force_newline of_block fmt blocks;
      pp_print_flush fmt ();
      pp_close_box fmt ()

end

let string_of_blocks (blocks : block list) : string =
  let open Format in
  let buf = Buffer.create 500 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 80;
  Printer.of_blocks fmt blocks;
  Buffer.contents buf
