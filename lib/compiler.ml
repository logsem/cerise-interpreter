open Convert
open Extract

exception CompilationException of string
let rec pp_of_instrs l =
  match l with
  | [] -> Printf.printf ""
  | i::l' ->
    Printf.printf "%s\n%!" (Pretty_printer.string_of_machine_op (Convert.translate_instr i));
  pp_of_instrs l'

let pp_sealable (sb : Ast.sealable) =
  match sb with
  | Cap (p, g, b, e, a) ->
    Printf.sprintf "(%s, %s, %s, %s, %s)" (Pretty_printer.string_of_perm p)
      (Pretty_printer.string_of_locality g)
      (Big_int_Z.string_of_big_int b)
      (Infinite_z.to_string e)
      (Big_int_Z.string_of_big_int a)
  | SealRange (p, g, b, e, a) ->
    Printf.sprintf "[%s, %s, %s, %s, %s]" (Pretty_printer.string_of_seal_perm p)
      (Pretty_printer.string_of_locality g)
      (Big_int_Z.string_of_big_int b)
      (Big_int_Z.string_of_big_int e)
      (Big_int_Z.string_of_big_int a)

let pp_word_reg (w : Ast.word)
  = match w with
  | I z -> Printf.sprintf "%s" (Big_int_Z.string_of_big_int z)
  | Sealable sb -> pp_sealable sb
  | Sealed (ot, sb) -> Printf.sprintf "{%s: %s}" (Big_int_Z.string_of_big_int ot) (pp_sealable sb)

let pp_word_asm (w : Ast.word)
  = match w with
  | I z -> (Pretty_printer.string_of_machine_op (Convert.translate_instr (Convert.machine_param.decodeInstr z)))
  | Sealable sb -> Printf.sprintf "#%s" (pp_sealable sb)
  | Sealed (ot, sb) -> Printf.sprintf "#{%s: %s}" (Big_int_Z.string_of_big_int ot) (pp_sealable sb)

let pp_regname (r : Extract.regName) =
  match r with
  | PC -> "PC"
  | STK -> "stk"
  | R n -> Printf.sprintf "r%s" (Big_int_Z.string_of_big_int n)

let machine_compile
    ?(addr_max = (Int32.to_int Int32.max_int)/4096 )
    ?(start_stack = Big_int_Z.big_int_of_int (addr_max/2) )
    program
  : (regName * Ast.word) list * (addr * Ast.word) list
  =
  match program Convert.machine_param start_stack with
  | Extract.Error m -> raise @@ CompilationException (Convert.convert_error_msg m)
  | Extract.Ok (regs, compiled_prog) ->
    let prog =
      List.map
        (fun w -> (fst w, Convert.translate_word (snd w)))
        compiled_prog
    in
    let regs =
      List.map
        (fun w -> (fst w, Convert.translate_word (snd w)))
        regs
    in
    (regs, prog)

let output_machine regfile_output_name asm_output_name regs prog  =
  let oc = open_out regfile_output_name in
  List.iter (fun w -> Printf.fprintf oc "%s := %s\n" (pp_regname (fst w))
                (pp_word_reg (snd w))) regs;
  close_out oc;
  let oc = open_out asm_output_name in
  List.iter (fun w -> Printf.fprintf oc "%s\n" (pp_word_asm (snd w))) prog;
  close_out oc
