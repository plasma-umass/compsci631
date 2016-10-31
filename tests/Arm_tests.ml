open Arm

let%TEST "label assembly" =
  let label = label "test_label" in
  string_of_assembly label = "test_label:"

let%TEST "add-cond assembly" =
  let add = add ~cond:EQ R0 (Reg R1) (Imm 12) in
  string_of_assembly add = "addeq r0,r1,#12"

let%TEST "ldr-index assembly" =
  let ldr = ldr ~index:(ix_lsl (Imm 2) 2) R0 R1 in
  print_endline (string_of_assembly ldr);
  string_of_assembly ldr = "ldr r0,[r1, +#8]"
