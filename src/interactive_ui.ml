open Notty
open Notty.Infix
open Cerise

type t = { application : Application_model.t; show_secondary : bool }
type event = Quit | Step | Step_ten | Undo | Move_primary of Z.t | Move_secondary of Z.t
  | Page_primary of int | Page_secondary of int | Follow_primary | Follow_secondary
  | Toggle_secondary | Cycle_capability | Resize of int * int

let create session = { application = Application_model.create session; show_secondary = true }
let application state = state.application
let status = function Machine_view.Running -> "Running" | Halted -> "Halted" | Failed -> "Failed"
let cap_style = A.(fg lightmagenta)
let sealed_style = A.(fg magenta)
let seal_range_style = A.(fg lightcyan)
let instruction_style = A.(fg green)
let address_style = A.(fg yellow)
let error_style = A.(fg red ++ st bold)
let word_style word = match word.Machine_view.kind with
  | Integer -> A.empty | Capability | Sentry -> cap_style | Sealed_capability -> sealed_style
  | Seal_range -> seal_range_style | Opaque -> A.(fg (gray 14))
let compact width text =
  if width <= 0 then "" else if String.length text <= width then text
  else if width <= 2 then String.sub text 0 width else String.sub text 0 (width - 2) ^ ".."
let image width attr text = I.hsnap ~align:`Left (max 0 width) (I.string attr (compact (max 0 width) text))
let hex address = Z.format "%X" address
let capability_suffix word = match word.Machine_view.capability with
  | None -> ""
  | Some c -> Printf.sprintf " %s%s %s-%s @%s" (String.concat "," c.permissions)
      (match c.locality with None -> "" | Some l -> "/" ^ l) (hex c.base) (hex c.limit) (hex c.cursor)
let word_text word = compact 46 (word.Machine_view.short_text ^ capability_suffix word)
let is_target address = function Some target -> Z.equal address target | None -> false
let register_cell width (r : Machine_view.register) =
  let label = r.label ^ ": " in
  I.hsnap ~align:`Left width
    (I.string A.empty label <|> image (max 0 (width - String.length label)) (word_style r.word) (word_text r.word))
let register_panel ~max_rows width registers =
  let cell_width = 32 in
  let columns = max 1 (width / cell_width) in
  let rows = min (max 1 max_rows) (max 1 ((List.length registers + columns - 1) / columns)) in
  let at index = List.nth_opt registers index in
  List.init rows (fun row ->
    List.init columns (fun col -> match at (col * rows + row) with
      | None -> I.empty | Some r -> register_cell (max 1 (width / columns)) r)
    |> List.fold_left ( <|> ) I.empty |> I.hsnap ~align:`Left width)
  |> List.fold_left ( <-> ) I.empty
let range_indicator capability address = match capability with
  | None -> "  "
  | Some c ->
      let boundary =
        if Z.equal c.Machine_view.base address then "┏"
        else if Z.equal Z.(c.Machine_view.limit - one) address then "┗"
        else if Z.compare c.Machine_view.base address <= 0 && Z.compare address c.Machine_view.limit < 0 then "┃"
        else " "
      in
      boundary ^ if Z.equal c.Machine_view.cursor address then "▶" else " "
let missing_word = { Machine_view.edit_text = "<unmapped>"; short_text = "<unmapped>"; detail_text = "<unmapped>";
  decoded_instruction = None; fingerprint = ""; kind = Opaque; integer = None; capability = None;
  sealing = None; annotations = [] }
let memory_line ~right ~active_capability view address width =
  let word = Option.value (Machine_view.memory_at address view) ~default:missing_word in
  let marker = range_indicator active_capability address in
  let instruction = Option.value word.decoded_instruction ~default:"" in
  let attr = if is_target address (Option.map (fun c -> c.Machine_view.cursor) active_capability) then error_style else word_style word in
  let line =
    I.string attr marker <|> I.string A.empty " " <|> I.string address_style (hex address)
    <|> I.string A.empty "  " <|> I.string attr (word_text word)
    <|> I.string A.empty "  " <|> I.string instruction_style instruction
    |> I.hsnap ~align:(if right then `Right else `Left) width
  in line
let rows_from view start count =
  List.init (max 0 count) (fun i -> Z.(start + of_int i))
  |> List.filter (fun a -> Z.compare a view.Machine_view.address_limit < 0)
let panel ~title ~right ~active_capability view start rows width =
  if width <= 0 then I.empty else
    let heading = image width A.(st bold) title in
    List.fold_left (fun out a -> out <-> memory_line ~right ~active_capability view a width) heading (rows_from view start rows)
let render_parts ~width ~height state =
  let view = Application_model.view state.application in
  let header = image width A.(st bold) (Printf.sprintf "%s  %s  [space step, n x10, backspace undo, tab follow, s panels, c cap, q quit]" view.backend_name (status view.status)) in
  if height <= 1 then (header, false, 0) else
    let regs = register_panel ~max_rows:(max 0 (height - 4)) width view.registers in
    let available = max 0 (height - 1 - I.height regs) in
    let two_panels = state.show_secondary && width >= 70 && available >= 2 in
    let rows = max 0 (available - 1) in
    let primary_width = if two_panels then width / 2 else width in
    let secondary_width = width - primary_width in
    let primary_capability =
      Option.bind
        (List.find_opt (fun (r : Machine_view.register) -> r.role = Machine_view.Program_counter) view.registers)
        (fun r -> r.word.capability)
    in
    let primary = panel ~title:"HEAP / PC" ~right:false ~active_capability:primary_capability view (Application_model.primary_start state.application) rows primary_width in
    let secondary_register = match List.find_opt (fun (r : Machine_view.register) -> r.role = Machine_view.Stack_pointer) view.registers with
      | Some r -> Some r | None -> Application_model.selected_capability state.application in
    let secondary_capability = Option.bind secondary_register (fun r -> r.word.capability) in
    let secondary_title = match secondary_register with
      | Some r when r.role = Machine_view.Stack_pointer -> "STACK / " ^ r.label
      | Some r -> "CAPABILITY / " ^ r.label
      | None -> "CAPABILITY / none" in
    let secondary = panel ~title:secondary_title ~right:true ~active_capability:secondary_capability view (Application_model.secondary_start state.application) rows secondary_width in
    let memories = if two_panels then primary <|> secondary else primary in
    (header <-> regs <-> memories, two_panels, rows)
let render ~width ~height state =
  let image, _, _ = render_parts ~width ~height state in
  I.vsnap ~align:`Top (max 0 height) (I.hsnap ~align:`Left (max 0 width) image)
let snapshot ~width ~height state =
  let output = Buffer.create (max 16 (width * height)) in
  Notty.Render.to_buffer output Notty.Cap.dumb (0, 0) (max 0 width, max 0 height)
    (render ~width ~height state);
  Buffer.contents output
let transition ~rows event state =
  let app = state.application in match event with
  | Quit -> None
  | Step ->
      Some
        (match Application_model.step app with
        | Ok app ->
            { state with application = Application_model.follow_secondary (Application_model.follow_primary app) }
        | Error _ -> state)
  | Step_ten ->
      Some
        (match Application_model.step_n 10 app with
        | Ok app ->
            { state with application = Application_model.follow_secondary (Application_model.follow_primary app) }
        | Error _ -> state)
  | Undo -> Some { state with application = Application_model.undo app }
  | Move_primary d -> Some { state with application = Application_model.move_primary d app }
  | Move_secondary d -> Some { state with application = Application_model.move_secondary d app }
  | Page_primary p -> Some { state with application = Application_model.page_primary rows p app }
  | Page_secondary p -> Some { state with application = Application_model.page_secondary rows p app }
  | Follow_primary -> Some { state with application = Application_model.follow_primary app }
  | Follow_secondary -> Some { state with application = Application_model.follow_secondary app }
  | Toggle_secondary -> Some { state with show_secondary = not state.show_secondary }
  | Cycle_capability -> Some { state with application = Application_model.select_next_capability app }
  | Resize _ -> Some state
let render_loop session =
  let terminal = Notty_unix.Term.create () in
  let rec loop state =
    let width, height = Notty_unix.Term.size terminal in
    Notty_unix.Term.image terminal (render ~width ~height state);
    let rows = max 1 (height / 2) in
    let apply event = match transition ~rows event state with None -> Notty_unix.Term.release terminal | Some next -> loop next in
    match Notty_unix.Term.event terminal with
    | `End | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> Notty_unix.Term.release terminal
    | `Key (`ASCII ' ', _) -> apply Step | `Key (`ASCII 'n', _) -> apply Step_ten | `Key (`Backspace, _) -> apply Undo
    | `Key (`ASCII 's', _) -> apply Toggle_secondary | `Key (`ASCII 'c', _) -> apply Cycle_capability
    | `Key (`Tab, modifiers) -> apply (if List.mem `Shift modifiers then Follow_secondary else Follow_primary)
    | `Key (`Arrow `Up, modifiers) -> apply (if List.mem `Ctrl modifiers then Move_secondary Z.minus_one else Move_primary Z.minus_one)
    | `Key (`Arrow `Down, modifiers) -> apply (if List.mem `Ctrl modifiers then Move_secondary Z.one else Move_primary Z.one)
    | `Key (`Arrow `Left, modifiers) -> let n = if List.mem `Shift modifiers then -10 else -1 in apply (if List.mem `Ctrl modifiers then Page_secondary n else Page_primary n)
    | `Key (`Arrow `Right, modifiers) -> let n = if List.mem `Shift modifiers then 10 else 1 in apply (if List.mem `Ctrl modifiers then Page_secondary n else Page_primary n)
    | `Key (`Page `Up, _) -> apply (Page_primary (-1)) | `Key (`Page `Down, _) -> apply (Page_primary 1)
    | `Mouse (`Press (`Scroll `Up), (x, _), modifiers) ->
        let page = List.mem `Ctrl modifiers in
        let secondary = state.show_secondary && width >= 70 && x >= width / 2 in
        apply (match secondary, page with
          | true, true -> Page_secondary (-1) | true, false -> Move_secondary Z.minus_one
          | false, true -> Page_primary (-1) | false, false -> Move_primary Z.minus_one)
    | `Mouse (`Press (`Scroll `Down), (x, _), modifiers) ->
        let page = List.mem `Ctrl modifiers in
        let secondary = state.show_secondary && width >= 70 && x >= width / 2 in
        apply (match secondary, page with
          | true, true -> Page_secondary 1 | true, false -> Move_secondary Z.one
          | false, true -> Page_primary 1 | false, false -> Move_primary Z.one)
    | `Resize (width, height) -> apply (Resize (width, height)) | _ -> loop state
  in loop (create session)
