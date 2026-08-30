(* The terminal UI is organized around three responsibilities: the shared state
   types below, pure layout/rendering in [Rendering], and event handling in
   [Controller]. The final aliases preserve the longstanding public facade. *)

open Notty
open Notty.Infix
open Cerise

type side = Left | Right

type t = {
  application : Application_model.t;
  show_secondary : bool;
  initial_primary_follow : bool;
  initial_secondary_follow : bool;
}

type event =
  | Quit
  | Step
  | Step_ten
  | Undo
  | Move_primary of Z.t
  | Move_secondary of Z.t
  | Page_primary of int
  | Page_secondary of int
  | Follow_primary
  | Follow_secondary
  | Toggle_secondary
  | Cycle_capability
  | Resize of int * int

(* UI state *)

let create (session : Machine_session.t) : t =
  {
    application = Application_model.create session;
    show_secondary = true;
    initial_primary_follow = true;
    initial_secondary_follow = true;
  }

let application (state : t) : Application_model.t = state.application

(* Pure text shaping, layout, and rendering *)

module Rendering = struct
  type fields = {
    address_width : int;
    range_width : int;
    permission_width : int;
    locality_width : int;
    sealable_width : int;
    word_width : int;
  }

  type layout = { dual : bool; primary_width : int; secondary_width : int; gap : int; rows : int }

  let cap_style : attr = A.(fg lightmagenta)
  let sealed_cap_style : attr = A.(fg magenta)
  let seal_range_style : attr = A.(fg lightcyan)
  let sealed_seal_range_style : attr = A.(fg cyan)
  let sealed_wrapper_style : attr = A.(fg (gray 14))
  let fallback_style : attr = sealed_wrapper_style
  let instruction_style : attr = A.(fg green)
  let address_style : attr = A.(fg yellow)
  let primary_indicator_style : attr = A.(fg red)
  let secondary_indicator_style : attr = A.(fg lightmagenta)

  let status_text (status : Machine_view.status) : string =
    match status with Machine_view.Running -> "Running" | Halted -> "Halted" | Failed -> "Failed"

  let status_style (status : Machine_view.status) : attr =
    match status with
    | Machine_view.Running -> A.empty
    | Halted -> A.(st bold)
    | Failed -> A.(fg red ++ st bold)

  let spaces (n : int) : string = String.make (max 0 n) ' '

  let pad_left (width : int) (value : string) : string =
    spaces (width - String.length value) ^ value

  let pad_right (width : int) (value : string) : string =
    value ^ spaces (width - String.length value)

  let compact (width : int) (value : string) : string =
    if width <= 0 then ""
    else if String.length value <= width then value
    else if width <= 2 then String.sub value 0 width
    else String.sub value 0 (width - 2) ^ ".."

  let middle_elide (width : int) (value : string) : string =
    if width <= 0 then ""
    else if String.length value <= width then value
    else if width <= 2 then String.sub value 0 width
    else
      let digits = width - 2 in
      let left = digits / 2 and right = digits - (digits / 2) in
      String.sub value 0 left ^ ".." ^ String.sub value (String.length value - right) right

  let snap_left (width : int) (image : image) : image = I.hsnap ~align:`Left (max 0 width) image
  let snap_right (width : int) (image : image) : image = I.hsnap ~align:`Right (max 0 width) image

  let text (width : int) (attr : attr) (value : string) : image =
    snap_left width (I.string attr (compact width value))

  let hex (value : Z.t) : string = Z.format "%X" value
  let address_width (limit : Z.t) : int = max 1 (String.length (hex limit))

  let address_text (width : int) (value : Z.t) : string =
    let raw = hex value in
    if Z.sign value >= 0 && String.length raw <= width then pad_left width raw
    else middle_elide width raw

  let range_text (width : int) (base : Z.t) (limit : Z.t) : string =
    let base = address_text width base and limit = address_text width limit in
    let full = base ^ "-" ^ limit in
    let rec prefix (index : int) : int =
      if index < width && base.[index] = limit.[index] then prefix (index + 1) else index
    in
    let common = prefix 0 in
    if common > 2 && common < width then
      let short =
        String.sub base 0 common ^ "["
        ^ String.sub base common (width - common)
        ^ "-"
        ^ String.sub limit common (width - common)
        ^ "]"
      in
      if String.length short < String.length full then short else full
    else full

  let normalize_locality (value : string) : string option =
    match String.lowercase_ascii value with
    | "global" -> Some "Global"
    | "local" -> Some "Local"
    | "directed" -> Some "Directed"
    | _ -> None

  let permission_text (permissions : string list) : string option =
    match permissions with
    | [ permission ] -> Some permission
    | _ :: _ :: _ -> Some ("[" ^ String.concat " " permissions ^ "]")
    | _ -> None

  let seal_permission (sealing : Machine_view.sealing) : string option =
    match (sealing.can_seal, sealing.can_unseal) with
    | Some false, Some false -> Some "SO"
    | Some true, Some false -> Some "S"
    | Some false, Some true -> Some "U"
    | Some true, Some true -> Some "SU"
    | _ -> None

  let words (view : Machine_view.t) : Machine_view.word list =
    List.map (fun (register : Machine_view.register) -> register.word) view.Machine_view.registers
    @ List.map (fun (cell : Machine_view.memory_cell) -> cell.word) view.memory
    @ match view.missing_cell with Unmapped -> [] | Default word -> [ word ]

  let fields (view : Machine_view.t) : fields =
    let address_width = address_width view.Machine_view.address_limit in
    let permission_width =
      List.fold_left
        (fun width word ->
          match word.Machine_view.capability with
          | Some capability ->
              Option.fold ~none:width
                ~some:(fun value -> max width (max 15 (String.length value)))
                (if List.length capability.permissions > 1 then
                   permission_text capability.permissions
                 else None)
          | None -> width)
        5 (words view)
    in
    let has_locality =
      List.exists
        (fun word ->
          Option.is_some (Option.bind word.Machine_view.capability (fun c -> c.locality))
          || Option.is_some (Option.bind word.seal_range (fun r -> r.locality)))
        (words view)
    in
    let locality_width = if has_locality then 8 else 0 in
    let range_width = (2 * address_width) + 1 in
    let sealable_width =
      permission_width
      + (if locality_width = 0 then 0 else locality_width + 1)
      + 1 + range_width + 1 + address_width
    in
    let word_width = 1 + address_width + 2 + sealable_width + 1 in
    { address_width; range_width; permission_width; locality_width; sealable_width; word_width }

  let aligned (side : side) (width : int) (image : image) : image =
    match side with Left -> snap_right width image | Right -> snap_left width image

  let sealable_image (frame : fields) (side : side) ~(attr : attr) ~(permission : string)
      ~(locality : string option) ~(base : Z.t) ~(limit : Z.t) ~(cursor : Z.t) : image option =
    let locality =
      match locality with
      | None when frame.locality_width = 0 -> Some ""
      | None -> None
      | Some value -> Option.map (pad_right frame.locality_width) (normalize_locality value)
    in
    match locality with
    | None -> None
    | Some locality ->
        let permission = pad_right frame.permission_width permission in
        let group =
          permission
          ^ (if frame.locality_width = 0 then "" else " " ^ locality)
          ^ " "
          ^ pad_right frame.range_width (range_text frame.address_width base limit)
        in
        let cursor = address_text frame.address_width cursor in
        let image =
          match side with
          | Left ->
              snap_left frame.sealable_width (I.string attr group)
              </> snap_right frame.sealable_width (I.string attr cursor)
          | Right ->
              snap_left frame.sealable_width (I.string attr cursor)
              </> snap_right frame.sealable_width (I.string attr group)
        in
        Some image

  let fallback_word (frame : fields) (side : side) (word : Machine_view.word) : image =
    (* Backend-specific or incomplete semantic views still have a stable textual
       representation; falling back avoids inventing a misleading capability. *)
    aligned side frame.word_width
      (I.string fallback_style (middle_elide frame.word_width word.Machine_view.short_text))

  let word_image (frame : fields) (side : side) (word : Machine_view.word) : image =
    let finish (inner : image) : image = aligned side frame.word_width inner in
    match
      (word.Machine_view.kind, word.integer, word.capability, word.seal_range, word.sealing)
    with
    | Integer, Some integer, None, None, None ->
        finish (I.string A.empty (middle_elide frame.word_width (hex integer)))
    | (Capability | Sentry), None, Some capability, None, None -> (
        match permission_text capability.permissions with
        | None -> fallback_word frame side word
        | Some permission -> (
            match
              sealable_image frame side ~attr:cap_style ~permission ~locality:capability.locality
                ~base:capability.base ~limit:capability.limit ~cursor:capability.cursor
            with
            | Some image -> finish image
            | None -> fallback_word frame side word))
    | Seal_range, None, None, Some range, Some sealing when not sealing.is_sealed -> (
        match seal_permission sealing with
        | None -> fallback_word frame side word
        | Some permission -> (
            match
              sealable_image frame side ~attr:seal_range_style ~permission ~locality:range.locality
                ~base:range.base ~limit:range.limit ~cursor:range.cursor
            with
            | Some image -> finish image
            | None -> fallback_word frame side word))
    | Sealed_capability, None, capability, seal_range, Some sealing
      when sealing.is_sealed
           && Option.is_some sealing.object_type
           && Option.is_some capability <> Option.is_some seal_range -> (
        let payload =
          match (capability, seal_range) with
          | Some capability, None -> (
              match permission_text capability.permissions with
              | Some permission ->
                  sealable_image frame side ~attr:sealed_cap_style ~permission
                    ~locality:capability.locality ~base:capability.base ~limit:capability.limit
                    ~cursor:capability.cursor
              | None -> None)
          | None, Some range -> (
              match seal_permission sealing with
              | Some permission ->
                  sealable_image frame side ~attr:sealed_seal_range_style ~permission
                    ~locality:range.locality ~base:range.base ~limit:range.limit
                    ~cursor:range.cursor
              | None -> None)
          | _ -> None
        in
        match payload with
        | None -> fallback_word frame side word
        | Some payload ->
            let object_type = address_text frame.address_width (Option.get sealing.object_type) in
            finish
              (I.string sealed_wrapper_style ("{" ^ object_type ^ ": ")
              <|> payload <|> I.string sealed_wrapper_style "}"))
    | _ -> fallback_word frame side word

  let word_snapshot_with (capability : Cap.t) ~(address_limit : Z.t) ~(width : int) ~(side : side)
      (word : Machine_view.word) : string =
    let view =
      {
        Machine_view.backend_name = "test";
        status = Running;
        address_limit;
        pc = None;
        registers = [];
        enclave_table = None;
        memory = [ { address = Z.zero; word } ];
        missing_cell = Unmapped;
      }
    in
    let frame = fields view in
    let output = Buffer.create width in
    Render.to_buffer output capability (0, 0)
      (max 0 width, 1)
      (snap_left width (word_image frame side word));
    Buffer.contents output

  let word_snapshot ~(address_limit : Z.t) ~(width : int) ~(side : side) (word : Machine_view.word)
      : string =
    word_snapshot_with Cap.dumb ~address_limit ~width ~side word

  let word_ansi_snapshot ~(address_limit : Z.t) ~(width : int) ~(side : side)
      (word : Machine_view.word) : string =
    word_snapshot_with Cap.ansi ~address_limit ~width ~side word

  let missing_word : Machine_view.word =
    {
      Machine_view.edit_text = "<unmapped>";
      short_text = "<unmapped>";
      detail_text = "<unmapped>";
      decoded_instruction = None;
      fingerprint = "";
      kind = Opaque;
      integer = None;
      capability = None;
      seal_range = None;
      sealing = None;
      annotations = [];
    }

  let active_capability (register : Machine_view.register option) : Machine_view.capability option =
    Option.bind register (fun (register : Machine_view.register) -> register.word.capability)

  let in_bounds (capability : Machine_view.capability option) (address : Z.t) : bool =
    Option.fold ~none:false
      ~some:(fun (capability : Machine_view.capability) ->
        Z.compare address capability.base >= 0 && Z.compare address capability.limit < 0)
      capability

  let instruction_image (capability : Machine_view.capability option) (address : Z.t)
      (word : Machine_view.word) : image =
    if
      word.Machine_view.kind <> Integer || Option.is_none word.integer
      || not (in_bounds capability address)
    then I.empty
    else I.string instruction_style (Option.value word.decoded_instruction ~default:"???")

  let boundary ~(side : side) (capability : Machine_view.capability option) (address : Z.t) : string
      =
    match capability with
    | None -> " "
    | Some (capability : Machine_view.capability) ->
        if Z.equal address capability.base then match side with Left -> "┏" | Right -> "┓"
        else if Z.equal address Z.(capability.limit - one) then
          match side with Left -> "┗" | Right -> "┛"
        else if Z.compare address capability.base >= 0 && Z.compare address capability.limit < 0
        then "┃"
        else " "

  let memory_line (frame : fields) ~(side : side) ~(capability : Machine_view.capability option)
      (view : Machine_view.t) (address : Z.t) (width : int) : image =
    let word = Option.value (Machine_view.find_memory_word address view) ~default:missing_word in
    let indicator_style =
      match side with Left -> primary_indicator_style | Right -> secondary_indicator_style
    in
    let at_cursor =
      Option.fold ~none:false
        ~some:(fun (capability : Machine_view.capability) -> Z.equal address capability.cursor)
        capability
    in
    let cursor =
      match (side, at_cursor) with Left, true -> " ▶ " | Right, true -> " ◀ " | _ -> "   "
    in
    let range_marker = boundary ~side capability address in
    let address_image = I.string address_style (address_text frame.address_width address) in
    let semantic = word_image frame side word in
    let instruction = instruction_image capability address word in
    let line =
      match side with
      | Left ->
          I.string indicator_style range_marker
          <|> I.string indicator_style cursor <|> address_image <|> I.string A.empty "  "
          <|> semantic <|> I.string A.empty "  " <|> instruction
      | Right ->
          instruction <|> I.string A.empty "  " <|> semantic <|> I.string A.empty "  "
          <|> address_image <|> I.string indicator_style cursor
          <|> I.string indicator_style range_marker
    in
    match side with Left -> snap_left width line | Right -> snap_right width line

  let rows_from (view : Machine_view.t) (start : Z.t) (count : int) : Z.t list =
    List.init (max 0 count) (fun index -> Z.(start + of_int index))
    |> List.filter (fun address ->
        Z.sign address >= 0 && Z.compare address view.Machine_view.address_limit < 0)

  let panel (frame : fields) ~(title : string) ~(side : side)
      ~(capability : Machine_view.capability option) (view : Machine_view.t) (start : Z.t)
      (rows : int) (width : int) : image =
    let heading =
      match side with
      | Left -> text width A.empty title
      | Right -> snap_right width (I.string A.empty title)
    in
    List.fold_left
      (fun image address -> image <-> memory_line frame ~side ~capability view address width)
      heading (rows_from view start rows)

  let deduplicate (registers : Machine_view.register list) : Machine_view.register list =
    List.fold_left
      (fun unique (register : Machine_view.register) ->
        if
          List.exists
            (fun (other : Machine_view.register) ->
              Machine_view.Register_id.equal register.Machine_view.id other.id)
            unique
        then unique
        else unique @ [ register ])
      [] registers

  let prioritized_registers (application : Application_model.t)
      (registers : Machine_view.register list) (capacity : int) : Machine_view.register list * int =
    let registers = deduplicate registers in
    if capacity >= List.length registers then (registers, 0)
    else if capacity <= 1 then
      let kept =
        match
          List.find_opt
            (fun (register : Machine_view.register) -> register.role = Program_counter)
            registers
        with
        | Some register -> [ register ]
        | None -> ( match registers with [] -> [] | register :: _ -> [ register ])
      in
      (kept, max 0 (List.length registers - 1))
    else
      let stack = Application_model.active_stack_pointer application in
      let selected = Application_model.selected_capability application in
      let same (register : Machine_view.register) (candidate : Machine_view.register option) : bool
          =
        Option.fold ~none:false
          ~some:(fun (other : Machine_view.register) ->
            Machine_view.Register_id.equal register.Machine_view.id other.id)
          candidate
      in
      let category (predicate : Machine_view.register -> bool) : Machine_view.register list =
        List.filter predicate registers
      in
      let preferred =
        category (fun (register : Machine_view.register) -> register.role = Program_counter)
        @ category (fun register -> same register stack)
        @ category (fun register ->
            match register.Machine_view.role with Backend_specific _ -> true | _ -> false)
        @ category (fun register -> same register selected)
        |> deduplicate
      in
      let ordered =
        preferred
        @ List.filter
            (fun (register : Machine_view.register) ->
              not
                (List.exists
                   (fun (preferred : Machine_view.register) ->
                     Machine_view.Register_id.equal register.Machine_view.id
                       preferred.Machine_view.id)
                   preferred))
            registers
      in
      let keep = min (capacity - 1) (List.length ordered) in
      (List.filteri (fun index _ -> index < keep) ordered, List.length registers - keep)

  let register_panel (frame : fields) ~(max_rows : int) (width : int)
      (application : Application_model.t) (registers : Machine_view.register list) : image =
    if max_rows <= 0 || registers = [] then I.empty
    else
      let label_width =
        max 3
          (List.fold_left
             (fun width (register : Machine_view.register) ->
               max width (String.length register.label))
             0 registers)
      in
      let cell_width = label_width + 2 + frame.word_width + 2 in
      let columns = max 1 (width / cell_width) in
      let natural_rows = (List.length (deduplicate registers) + columns - 1) / columns in
      let rows = min max_rows (max 1 natural_rows) in
      let kept, omitted = prioritized_registers application registers (rows * columns) in
      let cells =
        List.map
          (fun (register : Machine_view.register) ->
            I.string A.empty (pad_left label_width register.label ^ ": ")
            <|> word_image frame Left register.word
            <|> I.string A.empty "  ")
          kept
        @
        if omitted > 0 then [ I.string fallback_style (Printf.sprintf "… +%d registers" omitted) ]
        else []
      in
      let at (index : int) : image option = List.nth_opt cells index in
      List.init rows (fun row ->
          List.init columns (fun column ->
              Option.value (at ((column * rows) + row)) ~default:I.empty)
          |> List.fold_left ( <|> ) I.empty |> snap_left width)
      |> List.fold_left ( <-> ) I.empty

  let enclave_panel (width : int) (max_rows : int) (table : Machine_view.enclave_table) : image =
    if max_rows <= 0 then I.empty
    else
      let heading = text width A.empty ("ENCLAVES  counter: " ^ hex table.counter) in
      if max_rows = 1 then heading
      else
        let entry_count = List.length table.entries in
        if max_rows = 2 then
          if entry_count = 0 then heading <-> text width fallback_style "<empty>"
          else heading <-> text width fallback_style (Printf.sprintf "… +%d enclave(s)" entry_count)
        else
          let id_width =
            List.fold_left
              (fun result (entry : Machine_view.enclave_table_entry) ->
                max result (String.length (hex entry.id)))
              (String.length "id") table.entries
          in
          let identity_width =
            List.fold_left
              (fun result (entry : Machine_view.enclave_table_entry) ->
                max result (String.length (hex entry.identity)))
              (String.length "identity") table.entries
          in
          let row (id : string) (identity : string) : image =
            text width A.empty (pad_right id_width id ^ "  " ^ pad_right identity_width identity)
          in
          let header = row "id" "identity" in
          if entry_count = 0 then heading <-> header <-> text width fallback_style "<empty>"
          else
            let data_rows = max_rows - 2 in
            let keep = if entry_count <= data_rows then entry_count else max 0 (data_rows - 1) in
            let entries =
              table.entries
              |> List.filteri (fun index _ -> index < keep)
              |> List.map (fun (entry : Machine_view.enclave_table_entry) ->
                  row (hex entry.id) (hex entry.identity))
            in
            let rows =
              if keep = entry_count then entries
              else
                entries
                @ [
                    text width fallback_style
                      (Printf.sprintf "… +%d enclave(s)" (entry_count - keep));
                  ]
            in
            List.fold_left ( <-> ) (heading <-> header) rows

  let status_row (width : int) (view : Machine_view.t) : image =
    let state_label = "machine state: " in
    let state_value = status_text view.Machine_view.status in
    let state = I.string A.empty state_label <|> I.string (status_style view.status) state_value in
    snap_right width state

  let footer_row (width : int) (view : Machine_view.t) : image =
    text width A.empty ("backend: " ^ view.backend_name)

  let secondary_register (application : Application_model.t) : Machine_view.register option =
    match Application_model.active_stack_pointer application with
    | Some register -> Some register
    | None -> Application_model.selected_capability application

  let compute_layout ~(width : int) ~(rows : int) ~(show_secondary : bool) (frame : fields) : layout
      =
    let minimum = 1 + 3 + frame.address_width + 2 + frame.word_width + 2 + 8 in
    let gap = 2 in
    let half = (width - gap) / 2 in
    (* A second pane is useful only if both halves retain all fixed semantic
       fields; narrower terminals adapt to one readable memory pane. *)
    let dual = show_secondary && rows > 0 && half >= minimum in
    if dual then { dual; primary_width = half; secondary_width = half; gap; rows }
    else { dual = false; primary_width = width; secondary_width = 0; gap = 0; rows }

  let display_application (state : t) (rows : int) : Application_model.t =
    let application =
      if state.initial_primary_follow then Application_model.follow_primary ~rows state.application
      else state.application
    in
    if state.initial_secondary_follow then Application_model.follow_secondary ~rows application
    else application

  let render_parts ~(width : int) ~(height : int) (state : t) : image * layout =
    let view = Application_model.view state.application in
    let frame = fields view in
    if width <= 0 || height <= 0 then
      (I.empty, compute_layout ~width:0 ~rows:0 ~show_secondary:false frame)
    else if height - 1 < 5 then
      (* Very short terminals retain status, nearby memory, and the footer instead
         of letting the normal multi-panel layout clip unpredictably. *)
      let body_height = height - 1 in
      let status = if body_height <= 0 then I.empty else status_row width view in
      let remaining = body_height - 1 in
      let primary_register =
        List.find_opt
          (fun (register : Machine_view.register) -> register.role = Program_counter)
          view.registers
      in
      let capability = active_capability primary_register in
      let memory_rows = if remaining = 1 then 1 else max 0 (remaining - 1) in
      let display = display_application state memory_rows in
      let heap =
        if remaining <= 0 then I.empty
        else if remaining = 1 then
          memory_line frame ~side:Left ~capability view
            (Application_model.primary_start display)
            width
        else
          panel frame ~title:"HEAP" ~side:Left ~capability view
            (Application_model.primary_start display)
            memory_rows width
      in
      let body = I.vsnap ~align:`Top (max 0 body_height) (status <-> heap) in
      ( body <-> footer_row width view,
        compute_layout ~width ~rows:memory_rows ~show_secondary:false frame )
    else
      let body_height = height - 1 in
      let enclave_heading_rows = if Option.is_some view.enclave_table then 1 else 0 in
      let registers =
        register_panel frame
          ~max_rows:(body_height - 5 - enclave_heading_rows)
          width state.application view.registers
      in
      let enclave =
        match view.enclave_table with
        | None -> I.empty
        | Some table -> enclave_panel width (body_height - I.height registers - 5) table
      in
      let memory_rows = max 3 (body_height - I.height registers - I.height enclave - 2) in
      let layout =
        compute_layout ~width ~rows:memory_rows ~show_secondary:state.show_secondary frame
      in
      let display = display_application state layout.rows in
      let primary_register =
        List.find_opt
          (fun (register : Machine_view.register) -> register.role = Program_counter)
          view.registers
      in
      let primary_capability = active_capability primary_register in
      let primary =
        panel frame ~title:"HEAP" ~side:Left ~capability:primary_capability view
          (Application_model.primary_start display)
          layout.rows layout.primary_width
      in
      let memories =
        if not layout.dual then primary
        else
          let secondary_register = secondary_register state.application in
          let secondary_capability = active_capability secondary_register in
          let title =
            match secondary_register with
            | Some register when register.role = Stack_pointer -> "STACK"
            | Some register -> "CAPABILITY " ^ register.label
            | None -> "CAPABILITY"
          in
          let secondary =
            panel frame ~title ~side:Right ~capability:secondary_capability view
              (Application_model.secondary_start display)
              layout.rows layout.secondary_width
          in
          primary <|> I.string A.empty (spaces layout.gap) <|> secondary
      in
      let body = registers <-> enclave <-> status_row width view <-> memories in
      (I.vsnap ~align:`Top body_height body <-> footer_row width view, layout)

  let render ~(width : int) ~(height : int) (state : t) : image =
    if width <= 0 || height <= 0 then I.empty
    else
      let image, _ = render_parts ~width ~height state in
      I.vsnap ~align:`Top height (snap_left width image)

  let snapshot_with (capability : Cap.t) ~(width : int) ~(height : int) (state : t) : string =
    let output = Buffer.create (max 16 (width * height)) in
    Render.to_buffer output capability (0, 0)
      (max 0 width, max 0 height)
      (render ~width ~height state);
    Buffer.contents output

  let snapshot ~(width : int) ~(height : int) (state : t) : string =
    snapshot_with Cap.dumb ~width ~height state

  let ansi_snapshot ~(width : int) ~(height : int) (state : t) : string =
    snapshot_with Cap.ansi ~width ~height state

  let scroll_event ~(width : int) ~(height : int) ~(x : int) ~(ctrl : bool)
      ~(direction : [< `Down | `Up ]) (state : t) : event =
    let _, layout = render_parts ~width ~height state in
    let amount = match direction with `Up -> -1 | `Down -> 1 in
    let secondary = layout.dual && x >= layout.primary_width + layout.gap in
    match (secondary, ctrl) with
    | true, true -> Page_secondary amount
    | true, false -> Move_secondary (Z.of_int amount)
    | false, true -> Page_primary amount
    | false, false -> Move_primary (Z.of_int amount)
end

(* State transitions and the impure terminal event loop *)

module Controller = struct
  let transition ~(rows : int) (event : event) (state : t) : t option =
    let application = state.application in
    match event with
    | Quit -> None
    | Step ->
        Some
          (match Application_model.step application with
          | Ok application ->
              {
                state with
                application =
                  application
                  |> Application_model.follow_primary ~rows
                  |> Application_model.follow_secondary ~rows;
                initial_primary_follow = false;
                initial_secondary_follow = false;
              }
          (* A failed request must not manufacture a new history entry. The
             session view remains responsible for displaying execution status. *)
          | Error _ -> state)
    | Step_ten ->
        Some
          (match Application_model.step_n 10 application with
          | Ok application ->
              {
                state with
                application =
                  application
                  |> Application_model.follow_primary ~rows
                  |> Application_model.follow_secondary ~rows;
                initial_primary_follow = false;
                initial_secondary_follow = false;
              }
          | Error _ -> state)
    | Undo ->
        let secondary_start = Application_model.secondary_start application in
        let application =
          Application_model.undo application |> Application_model.follow_primary ~rows
        in
        let application =
          Application_model.move_secondary
            Z.(secondary_start - Application_model.secondary_start application)
            application
        in
        Some { state with application; initial_primary_follow = false }
    | Move_primary delta ->
        Some
          {
            state with
            application = Application_model.move_primary delta application;
            initial_primary_follow = false;
          }
    | Move_secondary delta ->
        Some
          {
            state with
            application = Application_model.move_secondary delta application;
            initial_secondary_follow = false;
          }
    | Page_primary pages ->
        Some
          {
            state with
            application = Application_model.page_primary rows pages application;
            initial_primary_follow = false;
          }
    | Page_secondary pages ->
        Some
          {
            state with
            application = Application_model.page_secondary rows pages application;
            initial_secondary_follow = false;
          }
    | Follow_primary ->
        Some
          {
            state with
            application = Application_model.follow_primary ~rows application;
            initial_primary_follow = false;
          }
    | Follow_secondary ->
        Some
          {
            state with
            application = Application_model.follow_secondary ~rows application;
            initial_secondary_follow = false;
          }
    | Toggle_secondary ->
        let show_secondary = not state.show_secondary in
        let application =
          if show_secondary then Application_model.follow_secondary ~rows application
          else application
        in
        Some { state with application; show_secondary; initial_secondary_follow = false }
    | Cycle_capability ->
        let application = Application_model.select_next_capability application in
        let application =
          match Application_model.active_stack_pointer application with
          | Some _ -> application
          | None -> Application_model.follow_secondary ~rows application
        in
        Some { state with application; initial_secondary_follow = false }
    | Resize _ -> Some state

  let render_loop (session : Machine_session.t) : unit =
    let terminal = Notty_unix.Term.create () in
    let rec loop (state : t) : unit =
      let width, height = Notty_unix.Term.size terminal in
      let image, layout = Rendering.render_parts ~width ~height state in
      Notty_unix.Term.image terminal
        (I.vsnap ~align:`Top (max 0 height) (Rendering.snap_left width image));
      let rows = max 1 layout.rows in
      let apply (event : event) : unit =
        match transition ~rows event state with
        | None -> Notty_unix.Term.release terminal
        | Some next -> loop next
      in
      match Notty_unix.Term.event terminal with
      | `End | `Key (`Escape, _) | `Key (`ASCII 'q', _) -> Notty_unix.Term.release terminal
      | `Key (`ASCII ' ', _) -> apply Step
      | `Key (`ASCII 'n', _) -> apply Step_ten
      | `Key (`Backspace, _) -> apply Undo
      | `Key (`ASCII 's', _) -> apply Toggle_secondary
      | `Key (`ASCII 'c', _) -> apply Cycle_capability
      | `Key (`Tab, modifiers) ->
          apply (if List.mem `Shift modifiers then Follow_secondary else Follow_primary)
      | `Key (`Arrow `Up, modifiers) ->
          apply
            (if List.mem `Ctrl modifiers then Move_secondary Z.minus_one
             else Move_primary Z.minus_one)
      | `Key (`Arrow `Down, modifiers) ->
          apply (if List.mem `Ctrl modifiers then Move_secondary Z.one else Move_primary Z.one)
      | `Key (`Arrow `Left, modifiers) ->
          let pages = if List.mem `Shift modifiers then -10 else -1 in
          apply (if List.mem `Ctrl modifiers then Page_secondary pages else Page_primary pages)
      | `Key (`Arrow `Right, modifiers) ->
          let pages = if List.mem `Shift modifiers then 10 else 1 in
          apply (if List.mem `Ctrl modifiers then Page_secondary pages else Page_primary pages)
      | `Key (`Page `Up, _) -> apply (Page_primary (-1))
      | `Key (`Page `Down, _) -> apply (Page_primary 1)
      | `Mouse (`Press (`Scroll direction), (x, _), modifiers) ->
          ignore layout;
          apply
            (Rendering.scroll_event ~width ~height ~x ~ctrl:(List.mem `Ctrl modifiers) ~direction
               state)
      | `Resize (width, height) -> apply (Resize (width, height))
      | _ -> loop state
    in
    loop (create session)
end

let transition ~(rows : int) (event : event) (state : t) : t option =
  Controller.transition ~rows event state

let render ~(width : int) ~(height : int) (state : t) : image =
  Rendering.render ~width ~height state

let snapshot ~(width : int) ~(height : int) (state : t) : string =
  Rendering.snapshot ~width ~height state

let ansi_snapshot ~(width : int) ~(height : int) (state : t) : string =
  Rendering.ansi_snapshot ~width ~height state

let word_snapshot ~(address_limit : Z.t) ~(width : int) ~(side : side) (word : Machine_view.word) :
    string =
  Rendering.word_snapshot ~address_limit ~width ~side word

let word_ansi_snapshot ~(address_limit : Z.t) ~(width : int) ~(side : side)
    (word : Machine_view.word) : string =
  Rendering.word_ansi_snapshot ~address_limit ~width ~side word

let scroll_event ~(width : int) ~(height : int) ~(x : int) ~(ctrl : bool)
    ~(direction : [ `Up | `Down ]) (state : t) : event =
  Rendering.scroll_event ~width ~height ~x ~ctrl ~direction state

let render_loop (session : Machine_session.t) : unit = Controller.render_loop session
