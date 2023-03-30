open Libinterp

let rec pp_of_instrs l =
  match l with
  | [] -> Printf.printf ""
  | i::l' ->
    Printf.printf "%s\n%!" (Pretty_printer.string_of_machine_op (Convert.translate_instr i));
  pp_of_instrs l'

let () =
  let (((bank_module, bank_ftype), bank_locals), bank_expr) = Extract.bank_example in
  let compiled_bank = (Extract.compile Convert.driver bank_module bank_ftype bank_locals bank_expr) in
  match compiled_bank with
  | None -> failwith "compilation error"
  | Some prog ->
  (* This part of the code is very tied to the bank example, just a temporary
   workaround *)
  Printf.printf "data:\n\tjmp pc ; Dummy data for LT \ncode:\n";
  Printf.printf
    ";; manually prepare the stack\n
;; r29 contains a capability that points to the end of the stack\n
mov r1 pc
lea r1 -1
load r1 r1
storeU stk 0 0
storeU stk 0 0
storeU stk 0 r29
storeU stk 0 0
storeU stk 0 0
mov r29 0\n";
  pp_of_instrs prog;
  Printf.printf "end:";
