open Libinterp

let rec pp_of_instrs l =
  match l with
  | [] -> Printf.printf ""
  | i::l' ->
    Printf.printf "%s\n%!" (Pretty_printer.string_of_machine_op (Convert.translate_instr i));
  pp_of_instrs l'

let pp_w (w : Ast.word) format
  = match w with
  | I z -> (Pretty_printer.string_of_machine_op
                (Convert.translate_instr (Convert.machine_param.decodeInstr z)))
  | Cap (p, g, b, e, a) ->
    Printf.sprintf format (Pretty_printer.string_of_perm p)
      (Pretty_printer.string_of_locality g)
      (Big_int_Z.string_of_big_int b)
      (Big_int_Z.string_of_big_int e)
      (Big_int_Z.string_of_big_int a)

let pp_w2 (w : Ast.word) format
  = match w with
  | I z -> Printf.sprintf "%s" (Big_int_Z.string_of_big_int z)
    (* (Pretty_printer.string_of_machine_op *)
    (*             (Convert.translate_instr (Convert.driver.decodeInstr z))) *)
  | Cap (p, g, b, e, a) ->
    Printf.sprintf format (Pretty_printer.string_of_perm p)
      (Pretty_printer.string_of_locality g)
      (Big_int_Z.string_of_big_int b)
      (Big_int_Z.string_of_big_int e)
      (Big_int_Z.string_of_big_int a)

let pp_regname (r : Extract.regName) =
  match r with
  | PC -> "PC"
  | STK -> "stk"
  | R n -> Printf.sprintf "r%s" (Big_int_Z.string_of_big_int n)


let full_compile regfile_output_name asm_output_name program =
  let addr_max = (Int32.to_int Int32.max_int)/4096 in
  let start_stack = Big_int_Z.big_int_of_int (addr_max/2) in
  let end_stack = Big_int_Z.big_int_of_int addr_max in
  let (regs, compiled_prog) = program Convert.machine_param Convert.lmachine_param start_stack end_stack in
  let prog =
    List.map
      (fun w -> (fst w, Convert.translate_word (snd w)))
      compiled_prog
  in
  let oc = open_out regfile_output_name in
  List.iter (fun w -> Printf.fprintf oc "%s := %s\n" (pp_regname (fst w))
                (pp_w2 (Convert.translate_word (snd w)) "(%s, %s, %s, %s, %s)")) regs;
  close_out oc;
  let oc = open_out asm_output_name in
  List.iter (fun w -> Printf.fprintf oc "%s\n" (pp_w (snd w) "#(%s, %s, %s, %s, %s)")) prog;
  close_out oc


let () =
  full_compile
    "asm-toys/stack_loaded.reg"
    "asm-toys/stack_loaded.s"
    Extract.loaded_stack_example;
  full_compile
    "asm-toys/bank_loaded.reg"
    "asm-toys/bank_loaded.s"
    Extract.loaded_bank_example
