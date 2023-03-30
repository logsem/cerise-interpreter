open Libinterp

let rec pp_of_instrs l =
  match l with
  | [] -> Printf.printf ""
  | i::l' ->
    Printf.printf "%s\n%!" (Pretty_printer.string_of_machine_op (Convert.translate_instr i));
  pp_of_instrs l'

let () =
  let (((adv_module, adv_ftype), adv_locals), adv_expr) = Extract.adv_example in
  let compiled_adv = (Extract.compile Convert.driver adv_module adv_ftype adv_locals adv_expr) in
  match compiled_adv with
  | None -> failwith "compilation error"
  | Some prog ->
  Printf.printf
"_init:
_end_init:
_adv:\n";
  pp_of_instrs prog;
  Printf.printf "_adv_end:"
