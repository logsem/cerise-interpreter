open Cerise

let ok (matched_value : ('a, Diagnostic.t list) result) : 'a = match matched_value with
  | Ok x -> x
  | Error ds -> Alcotest.fail (String.concat "; " (List.map Diagnostic.message ds))

let golden =
  [("Jmp",0,1);("Jnz",1,1);("Move",2,2);("Load",4,1);("Store",5,2);
   ("Add",7,4);("Sub",11,4);("Lt",15,4);("Lea",19,2);("Restrict",21,2);
   ("SubSeg",23,4);("IsPtr",27,1);("GetP",28,1);("GetL",29,1);
   ("GetB",30,1);("GetE",31,1);("GetA",32,1);("Fail",33,1);("Halt",34,1);
   ("LoadU",35,2);("StoreU",37,4);("PromoteU",41,1)]

let allocations (() : unit) : unit =
  let expected_names =
    ["Jmp";"Jnz";"Move";"Load";"Store";"Add";"Sub";"Lt";"Lea";"Restrict";
     "SubSeg";"IsPtr";"GetP";"GetL";"GetB";"GetE";"GetA";"Fail";"Halt";
     "LoadU";"StoreU";"PromoteU"] in
  let check (name : string) (actual : (string * int * int) list) : unit =
    Alcotest.(check (list (triple string int int))) name golden actual in
  check "uCerise automatic allocation" Ucerise.Codec.allocations;
  check "mCerise automatic allocation" Mcerise.Codec.allocations;
  Alcotest.(check int) "frozen ISA case count" 22 (List.length expected_names);
  Alcotest.(check (list string)) "uCerise exact ISA order" expected_names
    (List.map (fun (name,_,_) -> name) Ucerise.Codec.allocations);
  Alcotest.(check (list string)) "mCerise exact ISA order" expected_names
    (List.map (fun (name,_,_) -> name) Mcerise.Codec.allocations)

let u_instructions =
  let open Ucerise.Ast in
  let r1=Reg 1 and r2=Reg 2 and rg=Register (Reg 3) and c=Constant (Z.of_int (-7)) in
  [Jmp r1;Jnz(r1,r2);Move(r1,rg);Move(r1,c);Load(r1,r2);Store(r1,rg);Store(r1,c);
   Add(r1,rg,rg);Add(r1,rg,c);Add(r1,c,rg);Add(r1,c,c);
   Sub(r1,rg,rg);Sub(r1,rg,c);Sub(r1,c,rg);Sub(r1,c,c);
   Lt(r1,rg,rg);Lt(r1,rg,c);Lt(r1,c,rg);Lt(r1,c,c);
   Lea(r1,rg);Lea(r1,c);Restrict(r1,rg);Restrict(r1,c);
   SubSeg(r1,rg,rg);SubSeg(r1,rg,c);SubSeg(r1,c,rg);SubSeg(r1,c,c);
   IsPtr(r1,r2);GetP(r1,r2);GetL(r1,r2);GetB(r1,r2);GetE(r1,r2);GetA(r1,r2);
   Fail;Halt;LoadU(r1,r2,rg);LoadU(r1,r2,c);
   StoreU(r1,rg,rg);StoreU(r1,rg,c);StoreU(r1,c,rg);StoreU(r1,c,c);PromoteU r1]

let m_instructions =
  let open Mcerise.Ast in
  let r1=Reg 1 and r2=Reg 2 and rg=Register (Reg 3) and c=Constant (Z.of_int (-7)) in
  [Jmp r1;Jnz(r1,r2);Move(r1,rg);Move(r1,c);Load(r1,r2);Store(r1,rg);Store(r1,c);
   Add(r1,rg,rg);Add(r1,rg,c);Add(r1,c,rg);Add(r1,c,c);
   Sub(r1,rg,rg);Sub(r1,rg,c);Sub(r1,c,rg);Sub(r1,c,c);
   Lt(r1,rg,rg);Lt(r1,rg,c);Lt(r1,c,rg);Lt(r1,c,c);
   Lea(r1,rg);Lea(r1,c);Restrict(r1,rg);Restrict(r1,c);
   SubSeg(r1,rg,rg);SubSeg(r1,rg,c);SubSeg(r1,c,rg);SubSeg(r1,c,c);
   IsPtr(r1,r2);GetP(r1,r2);GetL(r1,r2);GetB(r1,r2);GetE(r1,r2);GetA(r1,r2);
   Fail;Halt;LoadU(r1,r2,rg);LoadU(r1,r2,c);
   StoreU(r1,rg,rg);StoreU(r1,rg,c);StoreU(r1,c,rg);StoreU(r1,c,c);PromoteU r1]

let codecs (() : unit) : unit =
  List.iter (fun i ->
    let z=Result.get_ok (Ucerise.Codec.encode i) in
    Alcotest.(check bool) "u round trip" true (Result.get_ok (Ucerise.Codec.decode z)=i))
    u_instructions;
  List.iter (fun i ->
    let z=Result.get_ok (Mcerise.Codec.encode i) in
    Alcotest.(check bool) "m round trip" true (Result.get_ok (Mcerise.Codec.decode z)=i))
    m_instructions;
  List.iter (fun z ->
    Alcotest.(check bool) "u malformed rejected" true (Result.is_error (Ucerise.Codec.decode z));
    Alcotest.(check bool) "m malformed rejected" true (Result.is_error (Mcerise.Codec.decode z)))
    [Z.minus_one; Z.shift_left Z.one 10000]

let parser_matrix (() : unit) : unit =
  let source =
    "jmp r1\njnz r1 r2\nmove r1 -1\nload r1 r2\nstore r1 r2\nadd r1 r2 1\nsub r1 2 r2\nlt r1 r2 3\nlea r1 -1\nrestrict r1 (RW, GLOBAL)\nsubseg r1 0 4\nisptr r1 r2\ngetp r1 r2\ngetl r1 r2\ngetb r1 r2\ngete r1 r2\ngeta r1 r2\nfail\nhalt\nloadu r1 r2 -1\nstoreu r1 0 r2\npromoteu r1" in
  ignore (ok (Ucerise.Parser.parse_program source));
  ignore (ok (Mcerise.Parser.parse_program source));
  ignore (ok (Ucerise.Parser.parse_word "(URWLX, LOCAL, 1, 9, 4)"));
  ignore (ok (Mcerise.Parser.parse_word "(URWLX, DIRECTED, 1, 9, 4)"));
  let typed_macro (locality : string) : string =
    Printf.sprintf
      "%%macro typed(dst: reg, e: expr, v: value, p: perm, l: locality) move $dst $v add $dst $e 1 restrict $dst ($p, $l) # ($p, $l, $e, $e + 8, $e) %%endmacro %%typed(r1, 4, 9, URWL, %s) halt"
      locality in
  ignore (ok (Ucerise.Parser.parse_program (typed_macro "LOCAL")));
  ignore (ok (Mcerise.Parser.parse_program (typed_macro "DIRECTED")));
  let bad_unused =
    ["%macro bad(x: expr) move $x 1 %endmacro halt";
     "%macro bad(x: expr) move r1 $missing %endmacro halt";
     "%macro bad(x: expr) restrict r1 ($x, LOCAL) %endmacro halt";
     "%macro bad(x: expr) # (RW, LOCAL, 0, $missing, 0) %endmacro halt";
     "%macro bad(p: perm) # (RW, LOCAL, 0, $p, 0) %endmacro halt";
     "%macro bad(x: expr) # (RW, $x, 0, 8, 0) %endmacro halt"] in
  List.iter (fun source ->
    Alcotest.(check bool) "u rejects invalid unused typed macro" true
      (Result.is_error (Ucerise.Parser.parse_program source));
    Alcotest.(check bool) "m rejects invalid unused typed macro" true
      (Result.is_error (Mcerise.Parser.parse_program source))) bad_unused;
  let removed = ["Mul";"Rem";"Div";"Invoke";"GetOType";"GetWType";"Seal";"UnSeal"] in
  List.iter (fun op ->
    Alcotest.(check bool) ("u rejects "^op) true
      (Result.is_error (Ucerise.Parser.parse_program (op^" r1 r2 r3")));
    Alcotest.(check bool) ("m rejects "^op) true
      (Result.is_error (Mcerise.Parser.parse_program (op^" r1 r2 r3")))) removed;
  List.iter (fun word ->
    Alcotest.(check bool) ("u rejects "^word) true (Result.is_error (Ucerise.Parser.parse_word word));
    Alcotest.(check bool) ("m rejects "^word) true (Result.is_error (Mcerise.Parser.parse_word word)))
    ["[SU, GLOBAL, 0, 9, 1]";"{1: (RW, GLOBAL, 0, 9, 1)}";
     "{1: [SU, GLOBAL, 0, 9, 1]}"];
  List.iter (fun value ->
    let source = "move r1 "^value in
    Alcotest.(check bool) ("u rejects sealing value "^value) true
      (Result.is_error (Ucerise.Parser.parse_program source));
    Alcotest.(check bool) ("m rejects sealing value "^value) true
      (Result.is_error (Mcerise.Parser.parse_program source)))
    ["SO";"S";"U";"SU";"Int";"Cap";"SealRange";"Sealed"];
  Alcotest.(check bool) "u rejects sealing macro kind" true
    (Result.is_error (Ucerise.Parser.parse_program
      "%macro x(p: sealperm) move r1 $p %endmacro %x(S)"));
  Alcotest.(check bool) "m rejects sealing macro kind" true
    (Result.is_error (Mcerise.Parser.parse_program
      "%macro x(p: sealperm) move r1 $p %endmacro %x(S)"));
  Alcotest.(check bool) "u rejects Directed" true
    (Result.is_error (Ucerise.Parser.parse_word "(URWLX, DIRECTED, 1, 9, 4)"))

let nested_composite_macro_arguments (() : unit) : unit =
  let source =
    "%macro inner(v: value) restrict r1 $v %endmacro \
     %macro outer(p: perm, l: locality) %inner(($p, $l)) %endmacro \
     %outer(URWLX, Local) halt"
  in
  let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) () in
  let check_u (() : unit) : unit =
    match
      ok (Ucerise.Parser.parse_program source)
      |> Ucerise.Asm_ir.lower_program config |> ok
    with
    | Ucerise.Ast.I encoded :: _ ->
        let expected =
          Ucerise.Ast.Restrict
            ( Ucerise.Ast.Reg 1,
              Ucerise.Ast.Constant
                (Ucerise.Codec.encode_permission_locality Ucerise.Ast.URWLX Ucerise.Ast.Local) )
        in
        Alcotest.(check bool)
          "uCerise resolved nested permission/locality" true
          (Ucerise.Codec.decode encoded = Ok expected)
    | _ -> Alcotest.fail "uCerise did not lower the nested restriction"
  in
  let check_m (() : unit) : unit =
    match
      ok (Mcerise.Parser.parse_program source)
      |> Mcerise.Asm_ir.lower_program config |> ok
    with
    | Mcerise.Ast.I encoded :: _ ->
        let expected =
          Mcerise.Ast.Restrict
            ( Mcerise.Ast.Reg 1,
              Mcerise.Ast.Constant
                (Mcerise.Codec.encode_permission_locality Mcerise.Ast.URWLX Mcerise.Ast.Local) )
        in
        Alcotest.(check bool)
          "mCerise resolved nested permission/locality" true
          (Mcerise.Codec.decode encoded = Ok expected)
    | _ -> Alcotest.fail "mCerise did not lower the nested restriction"
  in
  check_u ();
  check_m ();
  let wrong_kind =
    "%macro inner(v: value) restrict r1 $v %endmacro \
     %macro outer(p: expr, l: locality) %inner(($p, $l)) %endmacro \
     %outer(1, Local) halt"
  in
  Alcotest.(check bool)
    "uCerise rejects wrong nested permission kind" true
    (Result.is_error (Ucerise.Parser.parse_program wrong_kind));
  Alcotest.(check bool)
    "mCerise rejects wrong nested permission kind" true
    (Result.is_error (Mcerise.Parser.parse_program wrong_kind))

let generated_construction_and_locations (() : unit) : unit =
  let source =
    "%define N 2 start: move r1 start + N %macro emit(x: expr) # $x %endmacro \
     %emit(N + 5) halt"
  in
  ignore (ok (Ucerise.Parser.parse_program source));
  ignore (ok (Mcerise.Parser.parse_program source));
  ignore (ok (Ucerise.Parser.parse_regfile "r1 := 7 r2 := (URWL, LOCAL, 0, 8, 1)"));
  ignore (ok (Mcerise.Parser.parse_regfile "r1 := 7 r2 := (URWL, DIRECTED, 0, 8, 1)"));
  let check_location (type value) (label : string) (expected_line : int)
      (expected_column : int) (matched_value : (value, Diagnostic.t list) result) : unit =
    match matched_value with
    | Error (diagnostic :: _) -> (
        match Diagnostic.location diagnostic with
        | Some location ->
            Alcotest.(check (option string)) (label ^ " filename") (Some "historical.s")
              location.source;
            Alcotest.(check int) (label ^ " line") expected_line location.line;
            Alcotest.(check int) (label ^ " column") expected_column location.column
        | None -> Alcotest.fail (label ^ " diagnostic has no source location"))
    | Error [] -> Alcotest.fail (label ^ " returned no diagnostics")
    | Ok _ -> Alcotest.fail (label ^ " unexpectedly parsed")
  in
  Ucerise.Parser.parse_program ~filename:"historical.s" "halt\n@"
  |> check_location "u lexer" 2 1;
  Mcerise.Parser.parse_program ~filename:"historical.s" "halt\nmove r1"
  |> check_location "m parser" 2 8;
  let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) () in
  let u_word = ok (Ucerise.Parser.parse_word "(URWLX, LOCAL, 1, 9, 4)") in
  let u_concrete = ok (Ucerise.Asm_ir.lower_word config u_word) in
  let u_round_trip =
    ok (Ucerise.Parser.parse_word (Ucerise.Printer.word u_concrete))
    |> Ucerise.Asm_ir.lower_word config |> ok
  in
  Alcotest.(check bool) "u word round trip" true (u_concrete = u_round_trip);
  let m_word = ok (Mcerise.Parser.parse_word "(URWLX, DIRECTED, 1, 9, 4)") in
  let m_concrete = ok (Mcerise.Asm_ir.lower_word config m_word) in
  let m_round_trip =
    ok (Mcerise.Parser.parse_word (Mcerise.Printer.word m_concrete))
    |> Mcerise.Asm_ir.lower_word config |> ok
  in
  Alcotest.(check bool) "m word round trip" true (m_concrete = m_round_trip)

let config = Runtime_config.create ~max_addr:(Z.of_int 64) ~stack_addr:(Z.of_int 32) ()
let session (backend : string) ?regfile:(regfile : string option) (source : string) : Machine_session.t =
  ok (Machine_session.create ~backend ~config ~source ~regfile)
let word (bank : Machine_view.register_bank) (key : string) (s : Machine_session.t) : Machine_view.word =
  (Option.get (Machine_view.find_register {Machine_view.Register_id.bank;key}
    (Machine_session.view s))).word
let int_reg (key : string) (s : Machine_session.t) : Z.t = Option.get (word Machine_view.General key s).integer
let cap (bank : Machine_view.register_bank) (key : string) (s : Machine_session.t) : Machine_view.capability = Option.get (word bank key s).capability
let run (s : Machine_session.t) : Machine_session.t = (Machine_session.run s).session

let semantics (() : unit) : unit =
  let u = session "ucerise" ~regfile:"r2 := (RW, GLOBAL, 0, 8, 0)"
    "isptr r1 r2 isptr r3 r4 halt" |> run in
  Alcotest.(check string) "IsPtr capability" "1" (Z.to_string (int_reg "r1" u));
  Alcotest.(check string) "IsPtr integer" "0" (Z.to_string (int_reg "r3" u));
  let u_exec = session "ucerise"
    ~regfile:"pc := (URWX, GLOBAL, 0, 2, 0)"
    "move r1 17 halt" |> run in
  Alcotest.(check string) "u URWX is executable" "17" (Z.to_string (int_reg "r1" u_exec));
  Alcotest.(check bool) "u non-executable URW PC fails" true
    ((Machine_session.view
        (session "ucerise" ~regfile:"pc := (URW, GLOBAL, 0, 2, 0)"
           "move r1 17 halt" |> run)).status=Machine_view.Failed);
  let load = session "ucerise" ~regfile:"r1 := (URWLX, LOCAL, 0, 8, 3)"
    "loadu r2 r1 -1 halt # 99" |> run in
  Alcotest.(check string) "LoadU below cursor" "99" (Z.to_string (int_reg "r2" load));
  let store = session "ucerise" ~regfile:"r1 := (URWLX, LOCAL, 5, 12, 5)"
    "storeu r1 0 42 halt" |> run in
  Alcotest.(check string) "StoreU advances cursor" "6"
    (Z.to_string (cap Machine_view.General "r1" store).cursor);
  let promoted = session "ucerise" ~regfile:"r1 := (URWLX, LOCAL, 5, 20, 9)"
    "promoteu r1 halt" |> run in
  let c=cap Machine_view.General "r1" promoted in
  Alcotest.(check (list string)) "PromoteU permission" ["RWLX"] c.permissions;
  Alcotest.(check string) "PromoteU truncates" "9" (Z.to_string c.limit);
  let bad = session "ucerise" ~regfile:"r1 := (RW, GLOBAL, 5, 12, 5)"
    "storeu r1 0 1 halt" |> run in
  Alcotest.(check bool) "StoreU permission failure" true
    ((Machine_session.view bad).status=Machine_view.Failed);
  let bad_load = session "ucerise" ~regfile:"r1 := (RWLX, LOCAL, 0, 8, 3)"
    "loadu r2 r1 -1 halt # 99" |> run in
  Alcotest.(check bool) "LoadU permission failure" true
    ((Machine_session.view bad_load).status=Machine_view.Failed);
  let bad_promote = session "ucerise" ~regfile:"r1 := (RWLX, LOCAL, 0, 8, 3)"
    "promoteu r1 halt" |> run in
  Alcotest.(check bool) "PromoteU permission failure" true
    ((Machine_session.view bad_promote).status=Machine_view.Failed);
  let local = session "ucerise" "getl r1 stk halt" |> run in
  Alcotest.(check string) "u initial Local stack"
    (Z.to_string (Ucerise.Codec.encode_locality Ucerise.Ast.Local))
    (Z.to_string (int_reg "r1" local));
  let localized = session "ucerise"
    ~regfile:"r2 := (URWLX, GLOBAL, 1, 9, 4)"
    "restrict r2 (URWLX, LOCAL) halt" |> run in
  Alcotest.(check (option string)) "u Global can restrict to Local" (Some "LOCAL")
    (cap Machine_view.General "r2" localized).locality;
  let directed = session "mcerise" "getl r1 stk halt" |> run in
  Alcotest.(check string) "m initial Directed stack"
    (Z.to_string (Mcerise.Codec.encode_locality Mcerise.Ast.Directed))
    (Z.to_string (int_reg "r1" directed));
  let m_exec = session "mcerise"
    ~regfile:"pc := (URWLX, DIRECTED, 0, 2, 0)"
    "move r1 19 halt" |> run in
  Alcotest.(check string) "m URWLX is executable" "19" (Z.to_string (int_reg "r1" m_exec));
  Alcotest.(check bool) "m non-executable URWL PC fails" true
    ((Machine_session.view
        (session "mcerise" ~regfile:"pc := (URWL, DIRECTED, 0, 2, 0)"
           "move r1 19 halt" |> run)).status=Machine_view.Failed);
  let m_u = session "mcerise"
    ~regfile:"r1 := (URWLX, DIRECTED, 0, 8, 5) r3 := (RW, DIRECTED, 0, 8, 0)"
    "loadu r2 r1 -1 promoteu r1 isptr r4 r3 halt # 99" |> run in
  Alcotest.(check string) "m LoadU" "99" (Z.to_string (int_reg "r2" m_u));
  Alcotest.(check (list string)) "m PromoteU" ["RWLX"]
    (cap Machine_view.General "r1" m_u).permissions;
  Alcotest.(check string) "m IsPtr" "1" (Z.to_string (int_reg "r4" m_u));
  let directed_store = session "mcerise" "storeu stk 0 42 halt" |> run in
  Alcotest.(check bool) "m directed StoreU succeeds" true
    ((Machine_session.view directed_store).status=Machine_view.Halted);
  List.iter (fun permission ->
    let stored = session "mcerise"
      ~regfile:(Printf.sprintf
        "r1 := (%s, DIRECTED, 0, 20, 10) r2 := (RW, DIRECTED, 0, 8, 5)"
        permission)
      "storeu r1 0 r2 halt" |> run in
    Alcotest.(check bool) ("m "^permission^" stores fitting Directed capability") true
      ((Machine_session.view stored).status=Machine_view.Halted))
    ["URWL";"URWLX"];
  let no_write_local = session "mcerise"
    ~regfile:"r1 := (URW, DIRECTED, 0, 20, 10) r2 := (RW, DIRECTED, 0, 8, 5)"
    "storeu r1 0 r2 halt" |> run in
  Alcotest.(check bool) "m URW rejects fitting Directed capability" true
    ((Machine_session.view no_write_local).status=Machine_view.Failed);
  let bad_directed = session "mcerise"
    ~regfile:"r1 := (URWLX, DIRECTED, 5, 20, 5) r2 := (RW, DIRECTED, 0, 20, 10)"
    "storeu r1 0 r2 halt" |> run in
  Alcotest.(check bool) "directed store read bound" true
    ((Machine_session.view bad_directed).status=Machine_view.Failed)

let sessions_and_edits (() : unit) : unit =
  let a=session "ucerise" "move r1 7 halt" and b=session "mcerise" "move r1 9 halt" in
  let a1=Result.get_ok (Machine_session.step a) in
  Alcotest.(check string) "interleaved u step" "7" (Z.to_string (int_reg "r1" a1));
  Alcotest.(check string) "m remains isolated" "0" (Z.to_string (int_reg "r1" b));
  let edited=ok (Machine_session.set_register_text {bank=Machine_view.General;key="r2"}
    "(URW, GLOBAL, 1, 8, 2)" a) in
  Alcotest.(check (list string)) "word edit" ["URW"]
    (cap Machine_view.General "r2" edited).permissions;
  Alcotest.(check (list string)) "registry order"
    ["vanilla";"cerise";"locality-cerise";"ucerise";"mcerise";"cerisier";"griotte";"griotte-extracted"] (Backend_registry.available_backend_names ())

let () =
  Alcotest.run "historical backends"
    [("isa",[Alcotest.test_case "allocations" `Quick allocations;
             Alcotest.test_case "codec matrix" `Quick codecs;
             Alcotest.test_case "parser matrix" `Quick parser_matrix;
             Alcotest.test_case "nested composite macro arguments" `Quick
               nested_composite_macro_arguments;
             Alcotest.test_case "generated construction and locations" `Quick
               generated_construction_and_locations]);
     ("machine",[Alcotest.test_case "semantics" `Quick semantics;
                 Alcotest.test_case "sessions and edits" `Quick sessions_and_edits])]
