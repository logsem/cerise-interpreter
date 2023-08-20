open Libmachine

let print_exec_state (m : Machine.mchn) =
  print_endline @@ Pretty_printer.string_of_exec_state (fst m)

let print_reg_state (m : Machine.mchn) =
  let open Pretty_printer in
  let rs = (snd m).reg in
  print_endline "+-----------------------";
  Machine.RegMap.iter (fun r w ->
    print_endline @@ string_of_reg_word r w) rs;
  print_endline "+-----------------------"

let interpreter (m_init : Machine.mchn) =
  let m_final = Machine.run m_init in
  print_reg_state m_final;
  Printf.printf "Final execution state: %s\n"
    (Pretty_printer.string_of_exec_state (fst m_final))
