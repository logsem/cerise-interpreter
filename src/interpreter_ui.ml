open Cerise

module Make (M : Legacy_machine_backend.S) = struct
let print_exec_state (m : M.t) =
  let state =
    match M.get_exec_state m with M.Running -> "Running" | M.Halted -> "Halted" | M.Failed -> "Failed"
  in
  print_endline state

let print_reg_state (m : M.t) =
  let open Pretty_printer in
  let rs = M.get_regfile m in
  print_endline "+-----------------------";
  M.RegMap.iter (fun r w -> print_endline @@ string_of_reg_word r w) rs;
  print_endline "+-----------------------"

let interpreter (m_init : M.t) =
  let m_final = M.run m_init in
  print_reg_state m_final;
  Printf.printf "Final execution state: %s\n" (match M.get_exec_state m_final with
    | M.Running -> "Running" | M.Halted -> "Halted" | M.Failed -> "Failed")
end
