(* Noninteractive output runs a session to completion and prints the final
   machine view in the stable, line-oriented CLI format. *)

open Cerise

let string_of_status (status : Machine_view.status) : string =
  match status with Machine_view.Running -> "Running" | Halted -> "Halted" | Failed -> "Failed"

let print_view (machine_view : Machine_view.t) : unit =
  Printf.printf "Backend: %s\n" machine_view.Machine_view.backend_name;
  print_endline "+ Registers";
  List.iter
    (fun (register : Machine_view.register) ->
      Printf.printf "%s: %s\n" register.label register.word.short_text)
    machine_view.registers;
  Printf.printf "State: %s\n" (string_of_status machine_view.status)

let interpreter (session : Machine_session.t) : unit =
  let execution = Machine_session.run session in
  print_view (Machine_session.view execution.session)
