open Libinterp

let rec pp_of_instrs l =
  match l with
  | [] -> Printf.printf ""
  | i::l' ->
    Printf.printf "%s\n%!" (Pretty_printer.string_of_machine_op (Convert.translate_instr i));
  pp_of_instrs l'

let pp_w (w : Ast.word)
  = match w with
  | I z -> (Pretty_printer.string_of_machine_op
                (Convert.translate_instr (Convert.driver.decodeInstr z)))
  | Cap (p, g, b, e, a) ->
    Printf.sprintf "#(%s, %s, %s, %s, %s)" (Pretty_printer.string_of_perm p)
      (Pretty_printer.string_of_locality g)
      (Big_int_Z.string_of_big_int b)
      (Big_int_Z.string_of_big_int e)
      (Big_int_Z.string_of_big_int a)


let () =

  let compiled_prog = Extract.linked_example Convert.driver in
  let prog =
    List.map
      (fun w -> (fst w, Convert.translate_word (snd w)))
      compiled_prog
  in
  Printf.printf "%s"
"storeU stk 0 0
storeU stk 0 0
storeU stk 0 r29
storeU stk 0 0
storeU stk 0 0
mov r29 0\n
lea pc 6\n";
  List.iter (fun w -> Printf.printf "%s\n" (pp_w (snd w))) prog
