open Convert
open Libmachine
open Misc
open Wasm


(* let glue_compile *)
(*     ?(addr_max = (Int32.to_int Int32.max_int)/4096 ) *)
(*     ?(start_stack = Big_int_Z.big_int_of_int (addr_max/2) ) *)
(*     program *)
(*   : (Ast.regname * Ast.word) list * (Z.t * Ast.word) list *)
(*   = *)
(*   let prog = program start_stack in *)
(*   machine_compile prog *)

let output_machine (regfile_output_name : string) (asm_output_name : string)
    (regs : (Ast.regname * Ast.word) list) prog  =
  let open Pretty_printer in
  let oc = open_out regfile_output_name in
  List.iter (fun w -> Printf.fprintf oc "%s := %s\n" (string_of_regname (fst w))
                (pp_raw_word (snd w))) regs;
  close_out oc;
  let oc = open_out asm_output_name in
  List.iter (fun w -> Printf.fprintf oc "%s\n" (pp_asm_word (snd w))) prog;
  close_out oc

(** [default_compiler] is a wrapper around [ConvertInterface.compile] with *)
(**  default compiler parameters *)
let default_compiler
    ?(addr_max = (Int32.to_int Int32.max_int)/4096 )
    ?(start_stack = Big_int_Z.big_int_of_int (addr_max/2) )
    (modules : linkable_unit_type list)
  =
  let ot_lm = Big_int_Z.big_int_of_int 0 in
  let ot_g = Big_int_Z.big_int_of_int 1 in
  let ot_sm = Big_int_Z.big_int_of_int 2 in
  let max_lin_mem = Big_int_Z.big_int_of_int 4 in
  let max_indirect_table = Big_int_Z.big_int_of_int 4 in
  let size_safe_mem = Big_int_Z.big_int_of_int 32 in

  (ConvertInterface.compile
    start_stack ot_lm ot_g ot_sm
    max_lin_mem max_indirect_table size_safe_mem) modules
