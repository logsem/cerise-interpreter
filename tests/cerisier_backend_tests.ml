open Cerise

let ok (matched_value : ('a, Diagnostic.t list) result) : 'a = match matched_value with
  | Ok value -> value
  | Error diagnostics ->
      Alcotest.fail (String.concat "; " (List.map Diagnostic.message diagnostics))

let config = Runtime_config.create ~max_addr:(Z.of_int 128) ~stack_addr:(Z.of_int 64) ()

let session ?regfile:(regfile : string option) (source : string) : Machine_session.t =
  ok (Machine_session.create ~backend:"cerisier" ~config ~source ~regfile)

let register (bank : Machine_view.register_bank) (key : string) (machine : Machine_session.t) : Machine_view.register =
  Machine_view.find_register { Machine_view.Register_id.bank; key } (Machine_session.view machine)
  |> Option.get

let integer (key : string) (machine : Machine_session.t) : Z.t = (register Machine_view.General key machine).word.integer |> Option.get
let capability (bank : Machine_view.register_bank) (key : string) (machine : Machine_session.t) : Machine_view.capability = (register bank key machine).word.capability |> Option.get
let run ?max_steps:(max_steps : int option) (machine : Machine_session.t) : Machine_session.t = (Machine_session.run ?max_steps machine).session

let parser_matrix (() : unit) : unit =
  let complete =
    "jmp r1 jnz r1 r2 mov r1 -1 load r1 r2 store r1 r2 add r1 r2 1 sub r1 2 r2 mul r1 r2 3 rem r1 \
     r2 4 div r1 r2 5 lt r1 r2 6 lea r1 -1 restrict r1 (URWLX, DIRECTED) subseg r1 0 MAX_ADDR getl \
     r1 r2 getb r1 r2 gete r1 r2 geta r1 r2 getp r1 r2 getotype r1 r2 getwtype r1 r2 seal r1 r2 r3 \
     unseal r1 r2 r3 invoke r1 r2 loadu r1 r2 -1 storeu r1 0 r2 promoteu r1 einit r1 r2 edeinit r1 \
     estoreid r1 r2 isunique r1 r2 fail halt"
  in
  ignore (ok (Cerisier.Parser.parse_program complete));
  List.iter
    (fun source -> ignore (ok (Cerisier.Parser.parse_word source)))
    [
      "(URWLX, DIRECTED, 0, MAX_ADDR, 4)";
      "[SU, GLOBAL, 0, 8, 1]";
      "{3: (RW, LOCAL, 0, 8, 1)}";
      "{4: [S, DIRECTED, 0, 8, 1]}";
    ];
  ignore
    (ok
       (Cerisier.Parser.parse_program
          "%macro enclave(dst: reg, src: reg, n: expr) einit $dst $src lea $dst $n %endmacro \
           %enclave(r1, r2, 2) halt"));
  List.iter
    (fun source -> ignore (ok (Cerisier.Parser.parse_program source)))
    [
      "inf: halt";
      "infinity: halt";
      "%define inf 3 halt";
      "%define infinity 3 halt";
      "%define inf 3 move r1 inf halt";
      "%define infinity 3 move r1 infinity halt";
    ];
  List.iter
    (fun source ->
      Alcotest.(check bool)
        ("reject " ^ source) true
        (Result.is_error (Cerisier.Parser.parse_program source)))
    [ "isptr r1 r2"; "jmper r1"; "movsr r1 r2" ];
  List.iter
    (fun (name, source) ->
      match Cerisier.Parser.parse_program source with
      | Error [ diagnostic ] ->
          Alcotest.(check string)
            (name ^ " is resolved as an ordinary identifier")
            (Printf.sprintf "Unknown label or integer definition %S." name)
            (Diagnostic.message diagnostic)
      | Error diagnostics ->
          Alcotest.failf "expected one resolution diagnostic for %s, got %d" name
            (List.length diagnostics)
      | Ok _ -> Alcotest.failf "expected unresolved identifier %s to fail" name)
    [
      ("inf", "move r1 inf");
      ("infinity", "move r1 infinity");
      ("inf", "# (RW, GLOBAL, 0, inf, 0)");
      ("infinity", "# (RW, GLOBAL, 0, infinity, 0)");
    ];
  List.iter
    (fun name ->
      let check_unresolved (type value) (label : string)
          (matched_value : (value, Diagnostic.t list) result) : unit = match matched_value with
        | Error [ diagnostic ] ->
            Alcotest.(check string)
              label
              (Printf.sprintf "an unresolved symbol %S remains" name)
              (Diagnostic.message diagnostic)
        | Error diagnostics ->
            Alcotest.failf "expected one unresolved-symbol diagnostic for %s, got %d" name
              (List.length diagnostics)
        | Ok _ -> Alcotest.failf "expected unresolved identifier %s to fail lowering" name
      in
      let word_source = Printf.sprintf "(RW, GLOBAL, 0, %s, 0)" name in
      let word = ok (Cerisier.Parser.parse_word word_source) in
      check_unresolved (name ^ " word remains unresolved")
        (Cerisier.Asm_ir.lower_word config word);
      let regfile_source = Printf.sprintf "r1 := %s" word_source in
      let regfile = ok (Cerisier.Parser.parse_regfile regfile_source) in
      check_unresolved (name ^ " regfile remains unresolved")
        (Cerisier.Asm_ir.lower_regfile config regfile))
    [ "inf"; "infinity" ];
  let located =
    match Cerisier.Parser.parse_program ~filename:"located.s" "halt\n@" with
    | Error (diagnostic :: _) -> (
        match Diagnostic.location diagnostic with
        | Some location -> location
        | None -> Alcotest.fail "expected a located lexer diagnostic")
    | _ -> Alcotest.fail "expected a located lexer diagnostic"
  in
  Alcotest.(check (option string)) "lexer diagnostic filename" (Some "located.s") located.source;
  Alcotest.(check int) "lexer diagnostic line" 2 located.line;
  let parser_location =
    match Cerisier.Parser.parse_program ~filename:"syntax.s" "jmp\n)" with
    | Error (diagnostic :: _) -> (
        match Diagnostic.location diagnostic with
        | Some location -> location
        | None -> Alcotest.fail "expected a located parser diagnostic")
    | _ -> Alcotest.fail "expected a located parser diagnostic"
  in
  Alcotest.(check (option string))
    "parser diagnostic filename" (Some "syntax.s") parser_location.source;
  Alcotest.(check int) "parser diagnostic line" 2 parser_location.line;
  List.iter
    (fun source ->
      let parsed = Cerisier.Parser.parse_word source |> ok in
      let concrete = Cerisier.Asm_ir.lower_word config parsed |> ok in
      let printed = Cerisier.Printer.word concrete in
      let reparsed = Cerisier.Parser.parse_word printed |> ok in
      let round_trip = Cerisier.Asm_ir.lower_word config reparsed |> ok in
      Alcotest.(check bool) ("word round trip " ^ source) true (concrete = round_trip))
    [ "-17"; "(URWLX, DIRECTED, 0, MAX_ADDR, 4)"; "[SU, GLOBAL, 0, 8, 1]";
      "{3: (RW, LOCAL, 0, 8, 1)}" ]

let instructions =
  let open Cerisier.Ast in
  let r1 = Reg 1
  and r2 = Reg 2
  and r3 = Reg 3
  and rr = Register (Reg 4)
  and c = Const (Z.of_int (-7)) in
  [
    Jmp r1;
    Jnz (r1, r2);
    Move (r1, rr);
    Move (r1, c);
    Load (r1, r2);
    Store (r1, rr);
    Store (r1, c);
    Add (r1, rr, rr);
    Add (r1, rr, c);
    Add (r1, c, rr);
    Add (r1, c, c);
    Sub (r1, rr, rr);
    Mul (r1, rr, c);
    Rem (r1, c, rr);
    Div (r1, c, c);
    Lt (r1, rr, rr);
    Lea (r1, rr);
    Lea (r1, c);
    Restrict (r1, c);
    SubSeg (r1, rr, c);
    GetL (r1, r2);
    GetB (r1, r2);
    GetE (r1, r2);
    GetA (r1, r2);
    GetP (r1, r2);
    GetOType (r1, r2);
    GetWType (r1, r2);
    Seal (r1, r2, r3);
    UnSeal (r1, r2, r3);
    Invoke (r1, r2);
    LoadU (r1, r2, c);
    StoreU (r1, rr, c);
    PromoteU r1;
    EInit (r1, r2);
    EDeInit r1;
    EStoreId (r1, r2);
    IsUnique (r1, r2);
    Fail;
    Halt;
  ]

let codec (() : unit) : unit =
  Alcotest.(check (list (triple string int int)))
    "fixed historical allocations"
    [
      ("Jmp", 0, 1);
      ("Jnz", 1, 1);
      ("Move", 2, 2);
      ("Load", 4, 1);
      ("Store", 5, 2);
      ("Add", 7, 4);
      ("Sub", 11, 4);
      ("Mul", 15, 4);
      ("Rem", 19, 4);
      ("Div", 23, 4);
      ("Lt", 27, 4);
      ("Lea", 31, 2);
      ("Restrict", 33, 2);
      ("SubSeg", 35, 4);
      ("GetL", 39, 1);
      ("GetB", 40, 1);
      ("GetE", 41, 1);
      ("GetA", 42, 1);
      ("GetP", 43, 1);
      ("GetOType", 44, 1);
      ("GetWType", 45, 1);
      ("Seal", 46, 1);
      ("UnSeal", 47, 1);
      ("Invoke", 48, 1);
      ("LoadU", 49, 2);
      ("StoreU", 51, 4);
      ("PromoteU", 55, 1);
      ("EInit", 56, 1);
      ("EDeInit", 57, 1);
      ("EStoreId", 58, 1);
      ("IsUnique", 59, 1);
      ("Fail", 60, 1);
      ("Halt", 61, 1);
    ]
    Cerisier.Codec.allocations;
  List.iter
    (fun instruction ->
      let encoded = Result.get_ok (Cerisier.Codec.encode instruction) in
      Alcotest.(check bool)
        "fixed codec round trip" true
        (Result.get_ok (Cerisier.Codec.decode encoded) = instruction))
    instructions;
  let open Cerisier.Ast in
  List.iter
    (fun (instruction, opcode) ->
      let encoded = Result.get_ok (Cerisier.Codec.encode instruction) in
      Alcotest.(check int) "historical low-byte opcode" opcode (Z.to_int (Z.extract encoded 0 8)))
    [
      (Jmp (Reg 1), 0x00);
      (EInit (Reg 1, Reg 2), 0x38);
      (EDeInit (Reg 1), 0x39);
      (EStoreId (Reg 1, Reg 2), 0x3a);
      (IsUnique (Reg 1, Reg 2), 0x3b);
      (Fail, 0x3c);
      (Halt, 0x3d);
    ];
  List.iter
    (fun (instruction, expected) ->
      Alcotest.(check string)
        "complete historical numeric encoding" (Z.to_string expected)
        (Z.to_string (Result.get_ok (Cerisier.Codec.encode instruction))))
    [
      (EInit (Reg 1, Reg 2), Z.of_int 14392);
      (EDeInit (Reg 1), Z.of_int 569);
      (EStoreId (Reg 1, Reg 2), Z.of_int 14394);
      (IsUnique (Reg 1, Reg 2), Z.of_int 14395);
      (Fail, Z.of_int 60);
      (Halt, Z.of_int 61);
      (Move (Reg 1, Const (Z.of_int (-7))), Z.of_int 47619);
      (Add (Reg 1, Const (Z.of_int (-7)), Register (Reg 2)), Z.of_int 11180041);
      (LoadU (Reg 1, Reg 2, Const (Z.of_int (-7))), Z.of_int 1419826);
    ];
  List.iter
    (fun encoded ->
      Alcotest.(check bool)
        "malformed decode is structured" true
        (Result.is_error (Cerisier.Codec.decode encoded)))
    [
      Z.minus_one;
      Z.of_int 0x3e;
      Z.of_int 0x13d;
      Z.shift_left Z.one 10000;
      Z.logor (Z.of_int 0x02) (Z.shift_left (Z.shift_left Z.one 100000) 8);
    ];
  List.iter
    (fun decode ->
      Alcotest.(check bool) "negative scalar rejected" true (Result.is_error (decode Z.minus_one)))
    [
      (fun z -> Result.map (fun _ -> ()) (Cerisier.Codec.decode_permission z));
      (fun z -> Result.map (fun _ -> ()) (Cerisier.Codec.decode_seal_permission z));
      (fun z -> Result.map (fun _ -> ()) (Cerisier.Codec.decode_word_type z));
      (fun z -> Result.map (fun _ -> ()) (Cerisier.Codec.decode_locality z));
      (fun z -> Result.map (fun _ -> ()) (Cerisier.Codec.decode_permission_locality z));
      (fun z -> Result.map (fun _ -> ()) (Cerisier.Codec.decode_seal_permission_locality z));
    ]

let finite_bounds_and_edits (() : unit) : unit =
  let initial = session "move r1 MAX_ADDR halt" in
  let stepped = Result.get_ok (Machine_session.step initial) in
  Alcotest.(check string) "MAX_ADDR evaluates finitely" "128" (Z.to_string (integer "r1" stepped));
  Alcotest.(check string) "original is immutable" "0" (Z.to_string (integer "r1" initial));
  let edited =
    ok
      (Machine_session.set_register_text
         { bank = Machine_view.General; key = "r2" }
         "(URWLX, DIRECTED, 1, MAX_ADDR, 4)" initial)
  in
  let cap = capability Machine_view.General "r2" edited in
  Alcotest.(check string) "edited finite limit" "128" (Z.to_string cap.limit);
  let stepped_n = Result.get_ok (Machine_session.step_n 2 initial) in
  Alcotest.(check bool)
    "step_n reaches halt" true
    ((Machine_session.view stepped_n).status = Machine_view.Halted);
  let view = Machine_session.view initial in
  Alcotest.(check string) "finite address limit" "128" (Z.to_string view.address_limit);
  Alcotest.(check bool) "sparse physical view" true (List.length view.memory = 2);
  Alcotest.(check bool)
    "logical zero missing cell" true
    (match Machine_view.find_memory_word (Z.of_int 100) view with
    | Some { integer = Some value; _ } -> Z.equal Z.zero value
    | Some _ -> false
    | None -> false);
  let memory_edited = ok (Machine_session.set_memory_text (Z.of_int 90) "41" initial) in
  Alcotest.(check string)
    "memory edit" "41"
    (match Machine_view.find_memory_word (Z.of_int 90) (Machine_session.view memory_edited) with
    | Some { integer = Some value; _ } -> Z.to_string value
    | _ -> Alcotest.fail "edited memory cell missing");
  Alcotest.(check bool)
    "memory edit is immutable" true
    (match Machine_view.find_memory_word (Z.of_int 90) (Machine_session.view initial) with
    | Some { integer = Some value; _ } -> Z.equal value Z.zero
    | _ -> false);
  Alcotest.(check string)
    "selected canonical name" "cerisier"
    (Machine_session.backend_name initial)

let parity_rules (() : unit) : unit =
  List.iter
    (fun permission ->
      let invalid_pc =
        session
          ~regfile:(Printf.sprintf "pc := (%s, DIRECTED, 0, 2, 0)" permission)
          "move r1 19 halt"
        |> run
      in
      Alcotest.(check bool)
        (permission ^ " PC fails") true
        ((Machine_session.view invalid_pc).status = Machine_view.Failed);
      Alcotest.(check string)
        (permission ^ " PC does not execute")
        "0"
        (Z.to_string (integer "r1" invalid_pc)))
    [ "URWLX"; "URWX" ];
  let restricted =
    session ~regfile:"r1 := (RWLX, DIRECTED, 0, 8, 1)" "restrict r1 (URWLX, DIRECTED) halt" |> run
  in
  Alcotest.(check (list string))
    "URWLX can be requested from RWLX" [ "URWLX" ]
    (capability Machine_view.General "r1" restricted).permissions;
  let bad_locality =
    session ~regfile:"r1 := (RW, LOCAL, 0, 8, 1)" "restrict r1 (RW, GLOBAL) halt" |> run
  in
  Alcotest.(check bool)
    "Local cannot become Global" true
    ((Machine_session.view bad_locality).status = Machine_view.Failed);
  let fitting =
    session ~regfile:"r1 := (URWLX, DIRECTED, 0, 20, 10) r2 := (RW, DIRECTED, 0, 8, 5)"
      "storeu r1 0 r2 halt"
    |> run
  in
  Alcotest.(check bool)
    "WL stores fitting Directed capability" true
    ((Machine_session.view fitting).status = Machine_view.Halted);
  let too_large =
    session ~regfile:"r1 := (URWLX, DIRECTED, 0, 20, 5) r2 := (RW, DIRECTED, 0, 20, 10)"
      "storeu r1 0 r2 halt"
    |> run
  in
  Alcotest.(check bool)
    "Directed readable bound enforced" true
    ((Machine_session.view too_large).status = Machine_view.Failed);
  let seal_range =
    session ~regfile:"r1 := (URW, DIRECTED, 0, 20, 5) r2 := [SU, DIRECTED, 0, 4, 0]"
      "storeu r1 0 r2 halt"
    |> run
  in
  Alcotest.(check bool)
    "Directed seal range requires WL" true
    ((Machine_session.view seal_range).status = Machine_view.Failed);
  let loose_subseg = session ~regfile:"r1 := (RW, GLOBAL, 0, 8, 1)" "subseg r1 7 40 halt" |> run in
  let loose = capability Machine_view.General "r1" loose_subseg in
  Alcotest.(check string)
    "historical SubSeg permits enlarged finite limit" "40" (Z.to_string loose.limit)

let einit_configured_region (() : unit) : unit =
  let open Cerisier.Ast in
  let b = Z.of_int 4 in
  let bounded_end = Z.pred (Runtime_config.max_addr config) in
  let oversized_end = Z.shift_left Z.one 100_000 in
  let make_state (e : Z.t) : Cerisier.Machine.t =
    let state = Cerisier.Machine.init config [] None in
    state
    |> Cerisier.Machine.set_register PC
         (Sealable (Cap (RX, Global, Z.zero, Z.of_int 2, Z.zero)))
    |> Cerisier.Machine.set_register (Reg 31) (I Z.zero)
    |> Cerisier.Machine.set_register (Reg 2) (Sealable (Cap (RX, Global, b, e, Z.of_int 5)))
    |> Cerisier.Machine.set_memory_raw b
         (Sealable (Cap (RW, Global, Z.of_int 2, Z.of_int 4, Z.of_int 2)))
    |> Cerisier.Machine.set_memory_raw (Z.of_int 3) (I (Z.of_int 99))
    |> Cerisier.Machine.set_memory_raw (Z.of_int 5) (I (Z.of_int 11))
    |> Cerisier.Machine.set_memory_raw (Z.of_int 7) (I (Z.of_int 22))
  in
  let execute (e : Z.t) : Cerisier.Machine.t = Cerisier.Machine.execute (EInit (Reg 1, Reg 2)) (make_state e) in
  let bounded = execute bounded_end
  and oversized = execute oversized_end
  and short = execute (Z.of_int 6) in
  let identity (state : Cerisier.Machine.t) : Z.t =
    Cerisier.Machine.ETableMap.find Z.zero state.enclave_table
  in
  let rec expected_region (address : Z.t) (words : word list) : word list =
    if address > bounded_end then List.rev words
    else
      let word =
        if Z.equal address (Z.of_int 5) then I (Z.of_int 11)
        else if Z.equal address (Z.of_int 7) then I (Z.of_int 22)
        else I Z.zero
      in
      expected_region (Z.succ address) (word :: words)
  in
  let expected_identity = Z.of_int (Hashtbl.hash (b, expected_region (Z.succ b) [])) in
  Alcotest.(check string)
    "ascending hash includes sparse holes and excludes the cell below b+1"
    (Z.to_string expected_identity) (Z.to_string (identity bounded));
  Alcotest.(check string)
    "oversized end has the bounded identity"
    (Z.to_string (identity bounded)) (Z.to_string (identity oversized));
  Alcotest.(check string)
    "explicit cell above e does not contribute"
    (Z.to_string (Z.of_int (Hashtbl.hash (b, [ I (Z.of_int 11); I Z.zero ]))))
    (Z.to_string (identity short));
  List.iter
    (fun (label, requested_end, state) ->
      match Cerisier.Machine.read_register (Reg 1) state with
      | Sealable (Cap (E, Global, base, limit, cursor)) ->
          Alcotest.(check string) (label ^ " base") (Z.to_string b) (Z.to_string base);
          Alcotest.(check string)
            (label ^ " retains requested end")
            (Z.to_string requested_end) (Z.to_string limit);
          Alcotest.(check string) (label ^ " cursor") "5" (Z.to_string cursor)
      | _ -> Alcotest.fail (label ^ " did not return an E capability"))
    [ ("bounded", bounded_end, bounded); ("oversized", oversized_end, oversized) ]

let fixture (name : string) : string =
  let local = "test_files/cerisier/pos/" ^ name in
  if Sys.file_exists local then local else "tests/" ^ local

let example (name : string) : Machine_session.t =
  let source = In_channel.with_open_bin (fixture name) In_channel.input_all in
  session source |> run ~max_steps:1000

let examples_and_lifecycle (() : unit) : unit =
  let unique = example "isunique.s" in
  Alcotest.(check bool)
    "isunique example halts" true
    ((Machine_session.view unique).status = Machine_view.Halted);
  Alcotest.(check string) "initial PC overlap is not unique" "0" (Z.to_string (integer "r5" unique));
  Alcotest.(check string) "separated region is unique" "1" (Z.to_string (integer "r6" unique));
  Alcotest.(check string) "stored alias is not unique" "0" (Z.to_string (integer "r7" unique));
  let enclave = example "enclave.s" in
  Alcotest.(check bool)
    "enclave lifecycle example halts" true
    ((Machine_session.view enclave).status = Machine_view.Halted);
  Alcotest.(check bool)
    "stored enclave identity is observable" true
    (not (Z.equal (integer "r3" enclave) Z.zero));
  let enclave_source = In_channel.with_open_bin (fixture "enclave.s") In_channel.input_all in
  let program =
    ok (Cerisier.Parser.parse_program enclave_source)
    |> Cerisier.Asm_ir.lower_program config |> ok
  in
  let direct = Cerisier.Machine.init config program None |> Cerisier.Machine.run in
  Alcotest.(check int)
    "EDeInit removes the enclave table entry" 0
    (Cerisier.Machine.ETableMap.cardinal direct.enclave_table);
  Alcotest.(check string) "enclave counter is monotone" "1" (Z.to_string direct.enclave_counter);
  let denied_init =
    session ~regfile:"r2 := (RX, GLOBAL, 3, 6, 4)"
      "einit r1 r2 halt # 0 # (RW, GLOBAL, 20, 30, 20) # 0 # 0"
    |> run
  in
  Alcotest.(check bool)
    "non-unique enclave code is unauthorized" true
    ((Machine_session.view denied_init).status = Machine_view.Failed);
  let unauthorized = session "estoreid r1 r2 halt" |> run in
  Alcotest.(check bool)
    "missing enclave ID fails" true
    ((Machine_session.view unauthorized).status = Machine_view.Failed)

let () =
  Alcotest.run "cerisier backend"
    [
      ( "frontend and codec",
        [
          Alcotest.test_case "parser matrix" `Quick parser_matrix;
          Alcotest.test_case "fixed codec" `Quick codec;
        ] );
      ( "machine",
        [
          Alcotest.test_case "finite bounds and edits" `Quick finite_bounds_and_edits;
          Alcotest.test_case "historical parity rules" `Quick parity_rules;
          Alcotest.test_case "EInit configured-memory region" `Quick einit_configured_region;
          Alcotest.test_case "examples and enclave lifecycle" `Quick examples_and_lifecycle;
        ] );
    ]
