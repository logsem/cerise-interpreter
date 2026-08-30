open Cerise

let ok (matched_value : ('a, Diagnostic.t list) result) : 'a =
  match matched_value with
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let integer (value : int) : Z.t = Z.of_int value
let config = Runtime_config.create ~max_addr:(integer 128) ~stack_addr:(integer 64) ()

let resolve_file (path : string) : string =
  if Sys.file_exists path then path else "../../../" ^ path

let read_file (path : string) : string =
  let path = resolve_file path in
  let channel = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let extracted_register (register : Griotte.Ast.register) : Griotte_extracted.Ast.register =
  match register with PC -> PC | Reg number -> Reg number

let extracted_operand (operand : Griotte.Ast.reg_or_const) : Griotte_extracted.Ast.reg_or_const =
  match operand with
  | Register register -> Register (extracted_register register)
  | Constant value -> Constant value

let extracted_permission ((rx, write, deep_local, deep_read_only) : Griotte.Ast.permission) :
    Griotte_extracted.Ast.permission =
  let rx = match rx with Orx -> Griotte_extracted.Ast.Orx | R -> R | X -> X | XSR -> XSR in
  let write = match write with Ow -> Griotte_extracted.Ast.Ow | W -> W | WL -> WL in
  let deep_local = match deep_local with DL -> Griotte_extracted.Ast.DL | LG -> LG in
  let deep_read_only = match deep_read_only with DRO -> Griotte_extracted.Ast.DRO | LM -> LM in
  (rx, write, deep_local, deep_read_only)

let extracted_locality (locality : Griotte.Ast.locality) : Griotte_extracted.Ast.locality =
  match locality with Local -> Local | Global -> Global

let extracted_word_type (word_type : Griotte.Ast.word_type) : Griotte_extracted.Ast.word_type =
  match word_type with
  | W_I -> W_I
  | W_Cap -> W_Cap
  | W_SealRange -> W_SealRange
  | W_Sealed -> W_Sealed
  | W_Sentry -> W_Sentry

let extracted_instruction : Griotte.Ast.instruction -> Griotte_extracted.Ast.instruction =
  let register = extracted_register and operand = extracted_operand in
  function
  | Jalr (a, b) -> Jalr (register a, register b)
  | Jmp target -> Jmp (operand target)
  | Jnz (condition, target) -> Jnz (register condition, operand target)
  | ReadSR (destination, MTDC) -> ReadSR (register destination, MTDC)
  | WriteSR (MTDC, source) -> WriteSR (MTDC, register source)
  | Move (destination, source) -> Move (register destination, operand source)
  | Load (destination, source) -> Load (register destination, register source)
  | Store (destination, source) -> Store (register destination, operand source)
  | Add (destination, left, right) -> Add (register destination, operand left, operand right)
  | Sub (destination, left, right) -> Sub (register destination, operand left, operand right)
  | Mul (destination, left, right) -> Mul (register destination, operand left, operand right)
  | LAnd (destination, left, right) -> LAnd (register destination, operand left, operand right)
  | LOr (destination, left, right) -> LOr (register destination, operand left, operand right)
  | LShiftL (destination, left, right) -> LShiftL (register destination, operand left, operand right)
  | LShiftR (destination, left, right) -> LShiftR (register destination, operand left, operand right)
  | Lt (destination, left, right) -> Lt (register destination, operand left, operand right)
  | Lea (destination, source) -> Lea (register destination, operand source)
  | Restrict (destination, source) -> Restrict (register destination, operand source)
  | SubSeg (destination, left, right) -> SubSeg (register destination, operand left, operand right)
  | GetL (destination, source) -> GetL (register destination, register source)
  | GetB (destination, source) -> GetB (register destination, register source)
  | GetE (destination, source) -> GetE (register destination, register source)
  | GetA (destination, source) -> GetA (register destination, register source)
  | GetP (destination, source) -> GetP (register destination, register source)
  | GetOType (destination, source) -> GetOType (register destination, register source)
  | GetWType (destination, source) -> GetWType (register destination, register source)
  | Seal (destination, source, sealing) ->
      Seal (register destination, register source, register sealing)
  | UnSeal (destination, source, sealing) ->
      UnSeal (register destination, register source, register sealing)
  | Fail -> Fail
  | Halt -> Halt

let instructions =
  let open Griotte.Ast in
  let a = Reg 1 and b = Reg 2 and r = Register (Reg 3) and c = Constant (integer (-7)) in
  [
    Jmp r;
    Jmp c;
    Jnz (a, r);
    Jnz (a, c);
    Jalr (a, b);
    ReadSR (a, MTDC);
    WriteSR (MTDC, a);
    Move (a, r);
    Move (a, c);
    Load (a, b);
    Store (a, r);
    Store (a, c);
    Add (a, r, r);
    Add (a, r, c);
    Add (a, c, r);
    Add (a, c, c);
    Sub (a, r, r);
    Sub (a, r, c);
    Sub (a, c, r);
    Sub (a, c, c);
    Mul (a, r, r);
    Mul (a, r, c);
    Mul (a, c, r);
    Mul (a, c, c);
    Lt (a, r, r);
    Lt (a, r, c);
    Lt (a, c, r);
    Lt (a, c, c);
    Lea (a, r);
    Lea (a, c);
    Restrict (a, r);
    Restrict (a, c);
    SubSeg (a, r, r);
    SubSeg (a, r, c);
    SubSeg (a, c, r);
    SubSeg (a, c, c);
    GetL (a, b);
    GetB (a, b);
    GetE (a, b);
    GetA (a, b);
    GetP (a, b);
    GetOType (a, b);
    GetWType (a, b);
    Seal (a, b, PC);
    UnSeal (a, b, PC);
    Fail;
    Halt;
    LAnd (a, r, r);
    LAnd (a, r, c);
    LAnd (a, c, r);
    LAnd (a, c, c);
    LOr (a, r, r);
    LOr (a, r, c);
    LOr (a, c, r);
    LOr (a, c, c);
    LShiftL (a, r, r);
    LShiftL (a, r, c);
    LShiftL (a, c, r);
    LShiftL (a, c, c);
    LShiftR (a, r, r);
    LShiftR (a, r, c);
    LShiftR (a, c, r);
    LShiftR (a, c, c);
  ]

let independent_instruction_codecs_and_metadata_identity (() : unit) : unit =
  List.iter
    (fun instruction ->
      let handwritten = Result.get_ok (Griotte.Codec.encode instruction) in
      let extracted_instruction = extracted_instruction instruction in
      let extracted = Result.get_ok (Griotte_extracted.Codec.encode extracted_instruction) in
      Alcotest.(check bool)
        "handwritten round trip" true
        (Griotte.Codec.decode handwritten = Ok instruction);
      Alcotest.(check bool)
        "extracted round trip" true
        (Griotte_extracted.Codec.decode extracted = Ok extracted_instruction))
    instructions;
  let open Griotte_extracted.Ast in
  Alcotest.(check string)
    "negative Jmp golden" "-767"
    (Z.to_string (Result.get_ok (Griotte_extracted.Codec.encode (Jmp (Constant (integer (-3)))))));
  Alcotest.(check string)
    "Jalr golden" "14340"
    (Z.to_string (Result.get_ok (Griotte_extracted.Codec.encode (Jalr (Reg 1, Reg 2)))));
  Alcotest.(check string)
    "Move negative constant golden" "47624"
    (Z.to_string
       (Result.get_ok (Griotte_extracted.Codec.encode (Move (Reg 1, Constant (integer (-7)))))));
  let open Griotte.Ast in
  let permissions =
    List.concat_map
      (fun rx ->
        List.concat_map
          (fun w ->
            List.concat_map
              (fun dl -> List.map (fun dro -> (rx, w, dl, dro)) [ DRO; LM ])
              [ DL; LG ])
          [ Ow; W; WL ])
      [ Orx; R; X; XSR ]
  in
  List.iter
    (fun permission ->
      let extracted_permission = extracted_permission permission in
      Alcotest.(check string)
        "permission encoding identity"
        (Z.to_string (Griotte.Codec.encode_permission permission))
        (Z.to_string (Griotte_extracted.Codec.encode_permission extracted_permission));
      List.iter
        (fun locality ->
          let extracted_locality = extracted_locality locality in
          Alcotest.(check string)
            "permission/locality encoding identity"
            (Z.to_string (Griotte.Codec.encode_permission_locality permission locality))
            (Z.to_string
               (Griotte_extracted.Codec.encode_permission_locality extracted_permission
                  extracted_locality)))
        [ Local; Global ])
    permissions;
  List.iter
    (fun seal_permission ->
      Alcotest.(check string)
        "seal-permission encoding identity"
        (Z.to_string (Griotte.Codec.encode_seal_permission seal_permission))
        (Z.to_string (Griotte_extracted.Codec.encode_seal_permission seal_permission));
      List.iter
        (fun locality ->
          let extracted_locality = extracted_locality locality in
          Alcotest.(check string)
            "seal-permission/locality encoding identity"
            (Z.to_string (Griotte.Codec.encode_seal_permission_locality seal_permission locality))
            (Z.to_string
               (Griotte_extracted.Codec.encode_seal_permission_locality seal_permission
                  extracted_locality)))
        [ Local; Global ])
    [ (false, false); (false, true); (true, false); (true, true) ];
  List.iter
    (fun word_type ->
      let extracted_word_type = extracted_word_type word_type in
      Alcotest.(check string)
        "word-type encoding identity"
        (Z.to_string (Griotte.Codec.encode_word_type word_type))
        (Z.to_string (Griotte_extracted.Codec.encode_word_type extracted_word_type)))
    [ W_I; W_Cap; W_SealRange; W_Sealed; W_Sentry ]

let large_codec_round_trip (() : unit) : unit =
  let open Griotte.Ast in
  let large_constant = Z.neg (Z.logor (Z.shift_left Z.one 100_019) (Z.of_int 0xa6)) in
  let instruction = Move (Reg 1, Constant large_constant) in
  let handwritten = Result.get_ok (Griotte.Codec.encode instruction) in
  let extracted_instruction = extracted_instruction instruction in
  let extracted = Result.get_ok (Griotte_extracted.Codec.encode extracted_instruction) in
  Alcotest.(check bool)
    "large finite handwritten round trip" true
    (Griotte.Codec.decode handwritten = Ok instruction);
  Alcotest.(check bool)
    "large finite independent round trip" true
    (Griotte_extracted.Codec.decode extracted = Ok extracted_instruction)

let decoder_totality (() : unit) : unit =
  let rejects (value : Z.t) : bool =
    match Griotte_extracted.Codec.decode value with
    | Error _ -> true
    | Ok _ -> false
    | exception _ -> false
  in
  Alcotest.(check bool) "negative malformed encoding" true (rejects Z.minus_one);
  (* Opcode Add, outer tuple (r1,-1): the inner pair is negative. *)
  let nested_negative = Z.logor (integer 0x0c) (Z.shift_left (integer 14) 8) in
  Alcotest.(check bool) "nested negative tuple" true (rejects nested_negative);
  let huge = Z.shift_left Z.one 20_000 in
  Alcotest.(check bool) "huge malformed register" true (rejects (Z.shift_left huge 8));
  List.iter
    (fun opcode ->
      Alcotest.(check bool)
        (Printf.sprintf "unallocated opcode 0x%02x rejected" opcode)
        true
        (rejects (integer opcode)))
    (List.init 8 (fun offset -> 0x18 + offset))

let parser_ownership_and_corpus (() : unit) : unit =
  let program_files =
    [
      "case_studies/counter.s";
      "case_studies/deep_immutability.s";
      "case_studies/deep_locality.s";
      "case_studies/kvs.s";
      "case_studies/lse.s";
      "case_studies/mutually_distrustful.s";
      "case_studies/stack_object.s";
      "case_studies/vae.s";
      "switcher/switcher.s";
      "switcher/switcher_commented.s";
      "switcher/switcher_example.s";
      "default/pos/deep_local.s";
      "default/pos/deep_ro.s";
      "default/pos/jmper_jalr.s";
      "default/neg/bad_movsr_noperm.s";
      "cli_smoke.s";
    ]
  in
  let regfile_files =
    [
      "case_studies/counter.reg";
      "case_studies/deep_immutability.reg";
      "case_studies/deep_locality.reg";
      "case_studies/kvs.reg";
      "case_studies/lse.reg";
      "case_studies/mutually_distrustful.reg";
      "case_studies/stack_object.reg";
      "case_studies/vae.reg";
      "switcher/switcher.reg";
    ]
  in
  let base = "tests/test_files/griotte/" in
  List.iter
    (fun relative ->
      let path = base ^ relative in
      match Griotte_extracted.Parser.parse_program ~filename:path (read_file path) with
      | Ok _ -> ()
      | Error diagnostics ->
          Alcotest.failf "extracted parser rejected %s: %s" path
            (String.concat "; " (List.map Diagnostic.to_string diagnostics)))
    program_files;
  List.iter
    (fun relative ->
      let path = base ^ relative in
      match Griotte_extracted.Parser.parse_regfile ~filename:path (read_file path) with
      | Ok _ -> ()
      | Error diagnostics ->
          Alcotest.failf "extracted parser rejected %s: %s" path
            (String.concat "; " (List.map Diagnostic.to_string diagnostics)))
    regfile_files;
  ignore
    (ok
       (Griotte_extracted.Parser.parse_program
          "%macro typed(dst: reg, e: expr, v: value, p: perm, sp: sealperm, l: locality, wt: \
           wtype) private: mov $dst $v restrict cgp ($p, $l) # ($p, $l, private, $e, 0) # [$sp, \
           $l, 0, 15, 0] mov ca1 $wt %endmacro %typed(ca0, 8, -2, [R WL LG LM], SU, Local, Sentry) \
           %typed(ca1, 9, -3, [R W DL DRO], S, Global, Cap) halt"));
  let original_word =
    ok
      (Griotte_extracted.Asm_ir.assemble_word config
         (ok (Griotte_extracted.Parser.parse_word "{3: ([R W DL DRO], Local, 0, 8, 1)}")))
  in
  let printed_word = Griotte_extracted.Printer.word original_word in
  let reparsed_word =
    ok
      (Griotte_extracted.Asm_ir.assemble_word config
         (ok (Griotte_extracted.Parser.parse_word printed_word)))
  in
  Alcotest.(check bool) "extracted word round trip" true (original_word = reparsed_word);
  let original_regfile =
    ok
      (Griotte_extracted.Asm_ir.assemble_regfile config
         (ok
            (Griotte_extracted.Parser.parse_regfile
               "cra := {3: ([R W DL DRO], Local, 0, 8, 1)} mtdc := [SU, Global, 0, 15, 2]")))
  in
  let registers, system_registers = original_regfile in
  let printed_regfile =
    List.map
      (fun (register, word) ->
        Printf.sprintf "%s := %s"
          (Griotte_extracted.Printer.register register)
          (Griotte_extracted.Printer.word word))
      registers
    @ List.map
        (fun (register, word) ->
          Printf.sprintf "%s := %s"
            (Griotte_extracted.Printer.system_register register)
            (Griotte_extracted.Printer.word word))
        system_registers
    |> String.concat " "
  in
  let reparsed_regfile =
    ok
      (Griotte_extracted.Asm_ir.assemble_regfile config
         (ok (Griotte_extracted.Parser.parse_regfile printed_regfile)))
  in
  Alcotest.(check bool) "extracted regfile round trip" true (original_regfile = reparsed_regfile);
  match Griotte_extracted.Parser.parse_program ~filename:"extracted-located.griotte" "halt\n@" with
  | Error (diagnostic :: _) ->
      Alcotest.(check bool)
        "extracted lexer failure is located" true
        (match Diagnostic.location diagnostic with
        | Some location ->
            location.source = Some "extracted-located.griotte"
            && location.line = 2 && location.column = 1
        | None -> false)
  | Error [] -> Alcotest.fail "extracted located failure returned no diagnostic"
  | Ok _ -> Alcotest.fail "extracted parser accepted an invalid character"

let nested_composite_macro_arguments (() : unit) : unit =
  let nested (kind : string) (argument : string) : string =
    Printf.sprintf
      "%%macro inner(v: value) restrict cgp $v %%endmacro %%macro outer(p: %s, l: locality) \
       %%inner(($p, $l)) %%endmacro %%outer(%s, Global) halt"
      kind argument
  in
  let handwritten (source : string) : Z.t =
    match ok (Griotte.Parser.parse_program source) with
    | [ Griotte.Asm_ir.Op instruction; Griotte.Asm_ir.Op Griotte.Asm_ir.Halt_term ] -> (
        match ok (Griotte.Asm_ir.assemble_instruction config instruction) with
        | Griotte.Ast.Restrict (_, Griotte.Ast.Constant value) -> value
        | _ -> Alcotest.fail "handwritten Griotte assembled a different nested instruction")
    | _ -> Alcotest.fail "handwritten Griotte did not resolve the nested restriction"
  in
  let extracted (source : string) : Z.t =
    match ok (Griotte_extracted.Parser.parse_program source) with
    | [
     Griotte_extracted.Asm_ir.Op instruction;
     Griotte_extracted.Asm_ir.Op Griotte_extracted.Asm_ir.Halt_term;
    ] -> (
        match ok (Griotte_extracted.Asm_ir.assemble_instruction config instruction) with
        | Griotte_extracted.Ast.Restrict (_, Griotte_extracted.Ast.Constant value) -> value
        | _ -> Alcotest.fail "extracted Griotte assembled a different nested instruction")
    | _ -> Alcotest.fail "extracted Griotte did not resolve the nested restriction"
  in
  let check (label : string) (expected : Z.t) (source : string) : unit =
    let handwritten_value = handwritten source in
    let extracted_value = extracted source in
    Alcotest.(check string)
      (label ^ " extracted value") (Z.to_string expected) (Z.to_string extracted_value);
    Alcotest.(check string)
      (label ^ " backend equivalence") (Z.to_string handwritten_value) (Z.to_string extracted_value)
  in
  let permission =
    ( Griotte_extracted.Ast.R,
      Griotte_extracted.Ast.WL,
      Griotte_extracted.Ast.LG,
      Griotte_extracted.Ast.LM )
  in
  check "nested permission/locality"
    (Griotte_extracted.Codec.encode_permission_locality permission Griotte_extracted.Ast.Global)
    (nested "perm" "[R WL LG LM]");
  check "nested seal-permission/locality"
    (Griotte_extracted.Codec.encode_seal_permission_locality (true, true)
       Griotte_extracted.Ast.Global)
    (nested "sealperm" "SU");
  Alcotest.(check bool)
    "extracted rejects wrong nested permission kind" true
    (Result.is_error (Griotte_extracted.Parser.parse_program (nested "wtype" "Cap")))

let normalize_word (instruction : string option) (word : Machine_view.word) : Machine_view.word =
  match instruction with
  | None -> { word with decoded_instruction = None }
  | Some instruction ->
      {
        word with
        edit_text = instruction;
        short_text = instruction;
        detail_text = instruction;
        decoded_instruction = Some instruction;
        fingerprint = instruction;
        integer = Some Z.zero;
      }

let normalize (instruction_registers : (Machine_view.register_id * string) list)
    (instruction_addresses : (Z.t * string) list) (missing_instruction : string option)
    (view : Machine_view.t) : Machine_view.t =
  let registers =
    List.map
      (fun (register : Machine_view.register) ->
        let instruction =
          List.find_map
            (fun (id, instruction) ->
              if Machine_view.Register_id.equal id register.id then Some instruction else None)
            instruction_registers
        in
        { register with Machine_view.word = normalize_word instruction register.word })
      view.registers
  in
  let memory =
    List.map
      (fun (cell : Machine_view.memory_cell) ->
        let instruction =
          List.find_map
            (fun (address, instruction) ->
              if Z.equal address cell.address then Some instruction else None)
            instruction_addresses
        in
        { cell with Machine_view.word = normalize_word instruction cell.word })
      view.memory
  in
  let missing_cell =
    match view.missing_cell with
    | Machine_view.Unmapped -> Machine_view.Unmapped
    | Default word -> Default (normalize_word missing_instruction word)
  in
  { view with Machine_view.backend_name = "griotte"; registers; memory; missing_cell }

let compare_view (label : string) (handwritten : Machine_session.t) (extracted : Machine_session.t)
    : unit =
  let handwritten_view = Machine_session.view handwritten in
  let extracted_view = Machine_session.view extracted in
  let matching_instruction (left : Machine_view.word) (right : Machine_view.word) : string option =
    match (left.decoded_instruction, right.decoded_instruction) with
    | Some left, Some right when String.equal left right -> Some left
    | _ -> None
  in
  let instruction_registers =
    List.filter_map
      (fun (register : Machine_view.register) ->
        Option.bind
          (List.find_opt
             (fun (other : Machine_view.register) ->
               Machine_view.Register_id.equal register.id other.id)
             extracted_view.registers)
          (fun other ->
            Option.map
              (fun instruction -> (register.id, instruction))
              (matching_instruction register.word other.word)))
      handwritten_view.registers
  in
  let instruction_addresses =
    List.filter_map
      (fun (cell : Machine_view.memory_cell) ->
        Option.bind
          (List.find_opt
             (fun (other : Machine_view.memory_cell) -> Z.equal cell.address other.address)
             extracted_view.memory)
          (fun other ->
            Option.map
              (fun instruction -> (cell.address, instruction))
              (matching_instruction cell.word other.word)))
      handwritten_view.memory
  in
  let missing_instruction =
    match (handwritten_view.missing_cell, extracted_view.missing_cell) with
    | Default left, Default right -> matching_instruction left right
    | Unmapped, Unmapped | Unmapped, Default _ | Default _, Unmapped -> None
  in
  Alcotest.(check bool)
    label true
    (normalize instruction_registers instruction_addresses missing_instruction handwritten_view
    = normalize instruction_registers instruction_addresses missing_instruction extracted_view)

let sessions ?(regfile : string option) (source : string) : Machine_session.t * Machine_session.t =
  ( ok (Machine_session.create ~backend:"griotte" ~config ~source ~regfile),
    ok (Machine_session.create ~backend:"griotte-extracted" ~config ~source ~regfile) )

let differential_final ?(regfile : string option) (label : string) (source : string) :
    Machine_session.t * Machine_session.t =
  let rec loop (step_number : int) (handwritten : Machine_session.t) (extracted : Machine_session.t)
      : Machine_session.t * Machine_session.t =
    compare_view
      (Printf.sprintf "%s after %d instruction steps" label step_number)
      handwritten extracted;
    match (Machine_session.view handwritten).status with
    | Machine_view.Halted | Failed -> (handwritten, extracted)
    | Running when step_number >= 100 -> Alcotest.fail (label ^ " did not stop")
    | Running ->
        let handwritten = Result.get_ok (Machine_session.step handwritten) in
        let extracted = Result.get_ok (Machine_session.step extracted) in
        loop (step_number + 1) handwritten extracted
  in
  let handwritten, extracted = sessions ?regfile source in
  loop 0 handwritten extracted

let differential ?(regfile : string option) (label : string) (source : string) : unit =
  ignore (differential_final ?regfile label source)

let general_word (key : string) (session : Machine_session.t) : Machine_view.register =
  Option.get
    (Machine_view.find_register
       { Machine_view.Register_id.bank = General; key }
       (Machine_session.view session))

let check_terminal_integer (label : string) (expected : Z.t)
    ((handwritten, extracted) : Machine_session.t * Machine_session.t) : unit =
  List.iter
    (fun (backend, session) ->
      let view = Machine_session.view session in
      Alcotest.(check bool)
        (label ^ " " ^ backend ^ " halted")
        true
        (view.status = Machine_view.Halted);
      Alcotest.(check (option string))
        (label ^ " " ^ backend ^ " result")
        (Some (Z.to_string expected))
        (Option.map Z.to_string (general_word "ca0" session).word.integer))
    [ ("handwritten", handwritten); ("extracted", extracted) ]

let check_subseg_result (label : string) (expected_status : Machine_view.status)
    (expected_base : Z.t) (expected_limit : Z.t) (expected_cursor : Z.t)
    ((handwritten, extracted) : Machine_session.t * Machine_session.t) : unit =
  List.iter
    (fun (backend, session) ->
      let view = Machine_session.view session in
      Alcotest.(check bool) (label ^ " " ^ backend ^ " status") true (view.status = expected_status);
      let word = (general_word "ca0" session).word in
      let base, limit, cursor =
        match word.capability with
        | Some capability -> (capability.base, capability.limit, capability.cursor)
        | None ->
            let annotation (name : string) : Z.t =
              Z.of_string (Option.get (List.assoc_opt name word.annotations))
            in
            (annotation "base", annotation "limit", annotation "cursor")
      in
      Alcotest.(check string)
        (label ^ " " ^ backend ^ " base")
        (Z.to_string expected_base) (Z.to_string base);
      Alcotest.(check string)
        (label ^ " " ^ backend ^ " limit")
        (Z.to_string expected_limit) (Z.to_string limit);
      Alcotest.(check string)
        (label ^ " " ^ backend ^ " cursor")
        (Z.to_string expected_cursor) (Z.to_string cursor))
    [ ("handwritten", handwritten); ("extracted", extracted) ]

let architectural_pc = "pc := ([XSR Ow LG LM], Global, 0, MAX_ADDR, 0) "

let arithmetic_logic_control (() : unit) : unit =
  differential "arithmetic/logic/control"
    "mov ca0 20 mov ca1 6 add ca2 ca0 ca1 sub ca3 ca0 ca1 mul ca4 ca1 3 land ca5 7 3 lor ca6 4 1 \
     lshiftl ca7 1 4 lshiftr cs0 16 2 lt cs1 ca1 ca0 jnz cs1 2 fail jmp 2 fail halt";
  differential "jalr/sentry"
    ~regfile:(architectural_pc ^ "ca0 := (E-[X Ow LG LM], Global, 0, 4, 2)")
    "jalr cra ca0 fail halt"

let shift_boundaries (() : unit) : unit =
  differential_final "arbitrary-precision left shift" "lshiftl ca0 4611686018427387904 1 halt"
  |> check_terminal_integer "arbitrary-precision left shift" (Z.of_string "9223372036854775808");
  differential_final "arithmetic right shift" "lshiftr ca0 -1 1 halt"
  |> check_terminal_integer "arithmetic right shift" Z.minus_one;
  let handwritten, extracted =
    differential_final "signed and large shift counts"
      "lshiftl ca0 8 (-1)\n\
       lshiftr ca1 8 (-1)\n\
       lshiftr ca2 (-3) 1\n\
       lshiftl ca3 (-3) 1\n\
       lshiftl ca4 7 0\n\
       lshiftr ca5 (-1) 100000\n\
       lshiftl ca6 7 (-100000)\n\
       halt"
  in
  List.iter
    (fun (backend, session) ->
      Alcotest.(check bool)
        (backend ^ " signed shifts halt") true
        ((Machine_session.view session).status = Machine_view.Halted);
      List.iter
        (fun (register, expected) ->
          Alcotest.(check (option string))
            (backend ^ " " ^ register)
            (Some (Z.to_string expected))
            (Option.map Z.to_string (general_word register session).word.integer))
        [
          ("ca0", integer 4);
          ("ca1", integer 16);
          ("ca2", integer (-2));
          ("ca3", integer (-6));
          ("ca4", integer 7);
          ("ca5", Z.minus_one);
          ("ca6", Z.zero);
        ])
    [ ("handwritten", handwritten); ("extracted", extracted) ]

let memory_and_locality (() : unit) : unit =
  differential "load/store"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 20, 21, 20)")
    "store ca0 42 load ca1 ca0 halt";
  differential "deep-local load"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL DL LM], Global, 2, 3, 2)")
    "load ca1 ca0 halt # ([R WL LG LM], Global, 0, 8, 0)";
  differential "deep-read-only load"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG DRO], Global, 2, 3, 2)")
    "load ca1 ca0 halt # ([R WL LG LM], Global, 0, 8, 0)";
  differential "local store rejection"
    ~regfile:
      (architectural_pc
     ^ "ca0 := ([R W LG LM], Global, 20, 21, 20) ca1 := ([R WL LG LM], Local, 0, 8, 0)")
    "store ca0 ca1 halt"

let capabilities_and_sealing (() : unit) : unit =
  differential "restrict/subseg/lea/getters"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 1, 20, 4)")
    "restrict ca0 ([R W DL DRO], Local) subseg ca0 2 10 lea ca0 1 getl ca1 ca0 getb ca2 ca0 gete \
     ca3 ca0 geta ca4 ca0 getp ca5 ca0 getwtype ca6 ca0 getotype ca7 ca0 halt";
  differential "seal/unseal"
    ~regfile:
      (architectural_pc ^ "ca0 := ([R WL LG LM], Global, 0, 8, 2) ca3 := [SU, Global, 0, 15, 0]")
    "seal ca1 ca3 ca0 getotype ca2 ca1 getwtype ca4 ca1 unseal ca5 ca3 ca1 halt";
  differential "malformed capability Restrict immediate"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 0, 8, 0)")
    "restrict ca0 999 halt";
  differential "malformed capability Restrict register"
    ~regfile:(architectural_pc ^ "ca0 := ([R WL LG LM], Global, 0, 8, 0)")
    "mov ca1 999 restrict ca0 ca1 halt";
  differential "malformed seal-range Restrict"
    ~regfile:(architectural_pc ^ "ca3 := [SU, Global, 0, 15, 0]")
    "restrict ca3 999 halt"

let subseg_boundaries (() : unit) : unit =
  let capability = architectural_pc ^ "ca0 := ([R WL LG LM], Global, 1, 10, 4)" in
  let seal_range = architectural_pc ^ "ca0 := [SU, Global, 1, 10, 4]" in
  let check (label : string) (regfile : string) (source : string) (status : Machine_view.status)
      (base : int) (limit : int) : unit =
    differential_final label ~regfile source
    |> check_subseg_result label status (integer base) (integer limit) (integer 4)
  in
  check "capability SubSeg rejects enlarged limit" capability "subseg ca0 2 20 halt"
    Machine_view.Failed 1 10;
  check "seal-range SubSeg rejects enlarged limit" seal_range "subseg ca0 2 20 halt"
    Machine_view.Failed 1 10;
  check "capability SubSeg narrows" capability "subseg ca0 2 9 halt" Machine_view.Halted 2 9;
  check "seal-range SubSeg narrows" seal_range "subseg ca0 2 9 halt" Machine_view.Halted 2 9;
  check "capability SubSeg rejects non-finite base" capability "subseg ca0 2000000 9 halt"
    Machine_view.Failed 1 10;
  check "seal-range SubSeg rejects non-finite base" seal_range "subseg ca0 2000000 9 halt"
    Machine_view.Failed 1 10;
  (* Rocq's [isWithin] constrains each new endpoint against the corresponding
     old endpoint; it deliberately does not impose [new_base <= new_limit]. *)
  check "capability SubSeg preserves exact endpoint rule" capability "subseg ca0 8 2 halt"
    Machine_view.Halted 8 2;
  check "seal-range SubSeg preserves exact endpoint rule" seal_range "subseg ca0 8 2 halt"
    Machine_view.Halted 8 2

let system_halt_fail_and_malformed (() : unit) : unit =
  differential "system authorization"
    "mov ca0 77 writesr mtdc ca0 readsR ca1 mtdc mov cnull 99 halt";
  differential "system rejection" ~regfile:"pc := ([X Ow LG LM], Global, 0, 2, 0)"
    "readsr ca0 mtdc halt";
  differential "explicit fail" "fail";
  differential "malformed instruction word" "# 254";
  differential "non-integer instruction word" "# ([R W LG LM], Global, 0, 1, 0)";
  differential "missing sparse instruction"
    ~regfile:"pc := ([XSR Ow LG LM], Global, 0, MAX_ADDR, 10)" "halt"

let edits_and_boundaries (() : unit) : unit =
  let handwritten, extracted = sessions "halt" in
  let register = { Machine_view.Register_id.bank = General; key = "cra" } in
  let handwritten =
    ok (Machine_session.set_register_text register "([R WL LG LM], Local, 0, 8, 1)" handwritten)
  in
  let extracted =
    ok (Machine_session.set_register_text register "([R WL LG LM], Local, 0, 8, 1)" extracted)
  in
  compare_view "register edit" handwritten extracted;
  let handwritten =
    ok (Machine_session.set_memory_text (integer 17) "{3: [SU, Global, 0, 15, 2]}" handwritten)
  in
  let extracted =
    ok (Machine_session.set_memory_text (integer 17) "{3: [SU, Global, 0, 15, 2]}" extracted)
  in
  compare_view "memory edit and sealing metadata" handwritten extracted;
  let cnull = { Machine_view.Register_id.bank = General; key = "cnull" } in
  let extracted = ok (Machine_session.set_register_text cnull "99" extracted) in
  let cnull_word = Option.get (Machine_view.find_register cnull (Machine_session.view extracted)) in
  Alcotest.(check (option string))
    "cnull immutable" (Some "0")
    (Option.map Z.to_string cnull_word.word.integer);
  let mtdc = { Machine_view.Register_id.bank = System; key = "mtdc" } in
  let handwritten = ok (Machine_session.set_register_text mtdc "37" handwritten) in
  let extracted = ok (Machine_session.set_register_text mtdc "37" extracted) in
  compare_view "MTDC edit" handwritten extracted;
  let invalid_word = "([R WL LG LM], Global, 0, 2000001, 0)" in
  Alcotest.(check bool)
    "finite-value mismatch is diagnostic" true
    (Result.is_error (Machine_session.set_register_text register invalid_word extracted));
  let too_large = Runtime_config.create ~max_addr:(integer 2_000_001) ~stack_addr:(integer 64) () in
  Alcotest.(check bool)
    "configuration mismatch is diagnostic" true
    (Result.is_error
       (Machine_session.create ~backend:"griotte-extracted" ~config:too_large ~source:"halt"
          ~regfile:None));
  Alcotest.(check bool)
    "program finite mismatch is diagnostic" true
    (Result.is_error
       (Machine_session.create ~backend:"griotte-extracted" ~config
          ~source:"# ([R W LG LM], Global, 0, 2000001, 0)" ~regfile:None));
  Alcotest.(check bool)
    "regfile object-type mismatch is diagnostic" true
    (Result.is_error
       (Machine_session.create ~backend:"griotte-extracted" ~config ~source:"halt"
          ~regfile:(Some "cra := {2000001: ([R W LG LM], Global, 0, 8, 0)}")))

let unsupported_arithmetic_rejected (() : unit) : unit =
  List.iter
    (fun backend ->
      List.iter
        (fun mnemonic ->
          let source = "halt\n" ^ mnemonic ^ " ca0 8 2" in
          match
            Machine_session.create_with_filenames ~source_filename:"unsupported.griotte"
              ~regfile_filename:None ~backend ~config ~source ~regfile:None
          with
          | Error (diagnostic :: _) ->
              Alcotest.(check string)
                "backend-owned parser diagnostic"
                (Printf.sprintf "Unsupported Griotte instruction `%s`." mnemonic)
                (Diagnostic.message diagnostic);
              Alcotest.(check bool)
                "backend-owned parser diagnostic is located" true
                (match Diagnostic.location diagnostic with
                | Some location ->
                    location.source = Some "unsupported.griotte"
                    && location.line = 2 && location.column = 1
                | None -> false)
          | Error [] -> Alcotest.fail "parser returned an empty diagnostic list"
          | Ok _ -> Alcotest.failf "%s accepted unsupported instruction %s" backend mnemonic)
        [ "rem"; "div" ])
    [ "griotte"; "griotte-extracted" ]

let step_n_contract (() : unit) : unit =
  let handwritten, extracted = sessions "mov ca0 1 mov ca1 2 halt" in
  let handwritten = Result.get_ok (Machine_session.step_n 2 handwritten) in
  let extracted = Result.get_ok (Machine_session.step_n 2 extracted) in
  compare_view "positive step_n" handwritten extracted;
  let _, extracted = sessions "halt" in
  Alcotest.(check bool)
    "negative step_n is structured backend error" true
    (match Machine_session.step_n (-1) extracted with
    | Error (Machine_backend.Backend_error _) -> true
    | Error (Stopped _) | Ok _ -> false)

let () =
  Alcotest.run "extracted Griotte"
    [
      ( "codec",
        [
          Alcotest.test_case "independent instruction codecs and metadata identity" `Quick
            independent_instruction_codecs_and_metadata_identity;
          Alcotest.test_case "large finite independent round trips" `Quick large_codec_round_trip;
          Alcotest.test_case "malformed totality" `Quick decoder_totality;
        ] );
      ( "parser",
        [
          Alcotest.test_case "owned parser, macros, round trips, corpus" `Quick
            parser_ownership_and_corpus;
          Alcotest.test_case "nested composite macro arguments" `Quick
            nested_composite_macro_arguments;
        ] );
      ( "differential",
        [
          Alcotest.test_case "arithmetic, logic, and control" `Quick arithmetic_logic_control;
          Alcotest.test_case "shift boundaries" `Quick shift_boundaries;
          Alcotest.test_case "memory and locality" `Quick memory_and_locality;
          Alcotest.test_case "capabilities and sealing" `Quick capabilities_and_sealing;
          Alcotest.test_case "SubSeg boundaries" `Quick subseg_boundaries;
          Alcotest.test_case "system, terminal, malformed" `Quick system_halt_fail_and_malformed;
          Alcotest.test_case "edits and boundaries" `Quick edits_and_boundaries;
          Alcotest.test_case "step_n contract" `Quick step_n_contract;
          Alcotest.test_case "independent parsers reject unsupported arithmetic" `Quick
            unsupported_arithmetic_rejected;
        ] );
    ]
