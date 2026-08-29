open Cerise

let string_of_status = function
  | Machine_view.Running -> "Running"
  | Halted -> "Halted"
  | Failed -> "Failed"

let print_view view =
  Printf.printf "Backend: %s\n" view.Machine_view.backend_name;
  print_endline "+ Registers";
  List.iter
    (fun register -> Printf.printf "%s: %s\n" register.Machine_view.label register.word.short_text)
    view.registers;
  Printf.printf "State: %s\n" (string_of_status view.status)

let interpreter session =
  let result = Machine_session.run session in
  print_view (Machine_session.view result.session)
