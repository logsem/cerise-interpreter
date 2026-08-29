open Notty
open Notty.Infix
open Notty_unix
open Cerise

let status_text = function
  | Machine_view.Running -> "Running"
  | Halted -> "Halted"
  | Failed -> "Failed"

let row_of_register register = register.Machine_view.label ^ ": " ^ register.word.short_text

let memory_rows view start rows =
  let rec loop address count result =
    if count <= 0 || Z.compare address view.Machine_view.address_limit >= 0 then List.rev result
    else
      let value =
        match Machine_view.memory_at address view with Some word -> word.short_text | None -> "<unmapped>"
      in
      loop Z.(succ address) (count - 1) (Printf.sprintf "%s: %s" (Z.to_string address) value :: result)
  in
  loop start rows []

let image_of_lines width lines =
  List.fold_left
    (fun image line -> image <-> I.hsnap ~align:`Left width (I.string A.empty line))
    I.empty lines

let render_loop session =
  let terminal = Term.create () in
  let rec loop state =
    let width, height = Term.size terminal in
    let view = Application_model.view state in
    let registers = List.map row_of_register view.registers in
    let selected =
      match Application_model.selected_capability state with
      | None -> "secondary: no capability selected"
      | Some register -> "secondary: " ^ register.label
    in
    let header =
      Printf.sprintf "backend: %s  state: %s  [space step, n x10, backspace undo, tab follow, c capability, q quit]"
        view.backend_name (status_text view.status)
    in
    let register_rows, primary_rows, secondary_rows =
      Application_model.row_budget ~height ~register_count:(List.length registers)
    in
    let registers = CCList.take register_rows registers in
    let primary = memory_rows view (Application_model.primary_start state) primary_rows in
    let secondary = memory_rows view (Application_model.secondary_start state) secondary_rows in
    let lines =
      if height <= 0 then []
      else if height = 1 then [ header ]
      else if height = 2 then [ header; "memory" ]
      else if height = 3 then [ header; "memory"; selected ]
      else [ header ] @ registers @ [ "memory" ] @ primary @ [ selected; "followed memory" ] @ secondary
    in
    let navigation_rows = max 1 (max primary_rows secondary_rows) in
    Term.image terminal (image_of_lines width lines);
    let rec event () =
      match Term.event terminal with
      | `End | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> Term.release terminal
      | `Key (`ASCII ' ', _) -> (
          match Application_model.step state with Ok next -> loop (Application_model.follow_primary next) | Error _ -> loop state)
      | `Key (`ASCII 'n', _) -> (
          match Application_model.step_n 10 state with Ok next -> loop (Application_model.follow_primary next) | Error _ -> loop state)
      | `Key (`Backspace, _) -> loop (Application_model.undo state)
      | `Key (`Tab, modifiers) ->
          if List.mem `Shift modifiers then loop (Application_model.follow_secondary state)
          else loop (Application_model.follow_primary state)
      | `Key (`ASCII 'c', _) -> loop (Application_model.select_next_capability state)
      | `Key (`Arrow `Up, modifiers) ->
          if List.mem `Ctrl modifiers then loop (Application_model.move_secondary Z.minus_one state)
          else loop (Application_model.move_primary Z.minus_one state)
      | `Key (`Arrow `Down, modifiers) ->
          if List.mem `Ctrl modifiers then loop (Application_model.move_secondary Z.one state)
          else loop (Application_model.move_primary Z.one state)
      | `Key (`Arrow `Left, modifiers) ->
          let pages = if List.mem `Shift modifiers then -10 else -1 in
          if List.mem `Ctrl modifiers then loop (Application_model.page_secondary navigation_rows pages state)
          else loop (Application_model.page_primary navigation_rows pages state)
      | `Key (`Arrow `Right, modifiers) ->
          let pages = if List.mem `Shift modifiers then 10 else 1 in
          if List.mem `Ctrl modifiers then loop (Application_model.page_secondary navigation_rows pages state)
          else loop (Application_model.page_primary navigation_rows pages state)
      | `Key (`Page `Up, _) -> loop (Application_model.page_primary navigation_rows (-1) state)
      | `Key (`Page `Down, _) -> loop (Application_model.page_primary navigation_rows 1 state)
      | `Resize (_, _) -> loop state
      | _ -> event ()
    in
    event ()
  in
  loop (Application_model.create session)
