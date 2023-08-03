open Libinterp
open Libinterp.Pretty_printer
open Libinterp.Ast


let statement_eq (a : statement) (b : statement) = (a = b)
let pprint_statement = Fmt.of_to_string string_of_statement

let machine_op_eq (a : machine_op) (b : machine_op) = (a = b)
let pprint_machine_op = Fmt.of_to_string string_of_machine_op

let statement_tst = Alcotest.testable pprint_statement statement_eq
let machine_op_tst = Alcotest.testable pprint_machine_op machine_op_eq

module To_test = struct
  let lex_parse = fun x ->
    List.hd @@ Ir.translate_prog @@ Parser.main Lexer.token @@ Lexing.from_string x
  let enc_interleave a b = Ir.interleave_int (Z.of_string a) (Z.of_string b)
  let enc_int a b = Encode.encode_int_int a b
  let enc_split = Encode.split_int
  let enc_dec_int a b = Encode.decode_int @@ Encode.encode_int_int a b
  let enc_dec_op s = Encode.decode_machine_op @@ Encode.encode_machine_op s
end

let make_op_test ((input, expect) : string * statement) =
  Alcotest.test_case input `Quick
  (fun _ -> Alcotest.(check statement_tst)
      (Format.sprintf "Checking Lex/Parse of: %s" input)
      expect
      (To_test.lex_parse input))

let encode_const c =
  Const (Ir.encode_const [] (ConstExpr (IntLit c)))
let encode_perm p =
  Const (Ir.encode_const [] (Perm p))
let encode_seal_perm sp =
  Const (Ir.encode_const [] (SealPerm sp))
let encode_wtype wt =
  Const (Ir.encode_const [] (Wtype wt))

(* TODO add test cases for mul/rem/div *)
let instr_tests = [
  ("jmp pc", Op (Jmp PC));
  ("jmp r0", Op (Jmp (Reg 0)));
  ("jnz r1 r2", Op (Jnz (Reg 1, Reg 2)));
  ("mov pc 25", Op (Move (PC, encode_const (25))));
  ("mov r3 -25", Op (Move (Reg 3, encode_const (-25))));
  ("load r4 r5", Op (Load (Reg 4, Reg 5)));
  ("store r6 r7", Op (Store (Reg 6, Register (Reg 7))));
  ("add r8 (10-15) (-37)", Op (Add (Reg 8, encode_const (-5), encode_const (-37))));
  ("sub r9 6 28", Op (Sub (Reg 9, encode_const 6, encode_const 28)));
  ("lt r10 496 8128 ; perfect numbers are cool!",
   Op (Lt (Reg 10, encode_const 496, encode_const 8128)));
  ("lea r11 r12", Op (Lea (Reg 11, Register (Reg 12))));
  ("restrict r13 RX", Op (Restrict (Reg 13, (encode_perm RX))));
  ("restrict r14 S", Op (Restrict (Reg 14, (encode_seal_perm (true,false)))));
  ("subseg r15 pc r16", Op (SubSeg (Reg 15, Register PC, Register (Reg 16))));
  ("getb r21 r17", Op (GetB (Reg 21, Reg 17)));
  ("gete r22 r18", Op (GetE (Reg 22, Reg 18)));
  ("geta r23 r19", Op (GetA (Reg 23, Reg 19)));
  ("getp r24 r20", Op (GetP (Reg 24, Reg 20)));
  ("getotype r25 r26", Op (GetOType (Reg 25, Reg 26)));
  ("getwtype r27 r28", Op (GetWType (Reg 27, Reg 28)));
  ("seal r22 r29 r30", Op (Seal (Reg 22, Reg 29, Reg 30)));
  ("unseal r31 r26 r27", Op (UnSeal (Reg 31, Reg 26, Reg 27)));
  ("fail", Op (Fail));
  ("halt", Op (Halt));
  ("mov r3 Sealed", Op (Move (Reg 3, encode_wtype W_Sealed)));
  ("mov r13 SealRange", Op (Move (Reg 13, encode_wtype W_SealRange)));
  ("mov r23 Cap", Op (Move (Reg 23, encode_wtype W_Cap)));
  ("mov r30 Int", Op (Move (Reg 30, encode_wtype W_I)));
]

let z_tst =
  let open Z in
  let pprint_z = Fmt.of_to_string (format "%b") in
  let z_eq (a : t) (b : t) = (a = b) in
  Alcotest.testable pprint_z z_eq

let test_encode_interleave () =
  Alcotest.(check z_tst)
    "interleaves as expected"
    (Z.of_string "0b111001")
    (To_test.enc_interleave "0b101" "0b110") 

let test_encode_pos_pos_int () =
  Alcotest.(check z_tst)
    "positive integers"
    (Z.of_string "0b11100100")
    (To_test.enc_int (Z.of_string "0b101") (Z.of_string "0b110"))

let test_encode_pos_neg_int () =
  Alcotest.(check z_tst)
    "positive first integer and negative second"
    (Z.of_string "0b11100110")
    (To_test.enc_int (Z.of_string "0b101") (Z.neg @@ Z.of_string "0b110"))

let test_encode_neg_pos_int () =
  Alcotest.(check z_tst)
    "positive first integer and negative second"
    (Z.of_string "0b11100101")
    (To_test.enc_int (Z.neg @@ Z.of_string "0b101") (Z.of_string "0b110"))

let test_encode_neg_neg_int () =
  Alcotest.(check z_tst)
    "positive first integer and negative second"
    (Z.of_string "0b11100111")
    (To_test.enc_int (Z.neg @@ Z.of_string "0b101") (Z.neg @@ Z.of_string "0b110"))

let test_encode_split_int () =
  Alcotest.(check (pair z_tst z_tst))
    "splitting as expected"
    ((Z.of_string "0b101"), (Z.of_string "0b110"))
    (To_test.enc_split @@ Z.of_string "0b111001")

let test_encode_decode_pos_pos_int () =
  Alcotest.(check (pair z_tst z_tst))
    "decoding an encoded number should retrieve the encoded numbers"
    (Z.of_string "0b101", Z.of_string "0b110")
    (To_test.enc_dec_int (Z.of_string "0b101") (Z.of_string "0b110"))

let test_encode_decode_int_bulk ((a,b) : int * int) =
  let x = Z.of_int a
  and y = Z.of_int b in
  Alcotest.test_case
    (Format.sprintf "testing encode-decode of: %d, %d" a b) `Quick
    (fun _ -> Alcotest.(check (pair z_tst z_tst))
        "same output as input"
        (x,y)
        (To_test.enc_dec_int x y))

let test_int_pair_list = [
  (6, 28);
  (496, 8128);
  (-3, 10);
  (809281, -182884);
  (-554433, -9182);
]

let make_enc_dec_stm_tests (stm, test_name) =
  Alcotest.test_case
    test_name
    `Quick
    (fun _ ->
       Alcotest.(check machine_op_tst)
         "same statement"
         stm
         (To_test.enc_dec_op stm))

(* TODO add test cases for mul/rem/div *)
let test_enc_dec_stm_list = [
  (Jmp PC, "encode-decode Jmp PC");
  (Jmp (Reg 28), "encode-decode Jmp R28");
  (Jnz (Reg 6, Reg 28), "encode-decode Jnz R6 R28");
  (Move (PC, Register (Reg 7)), "encode-decode Move PC R7");
  (Move (PC, const (-35)), "encode-decode Move PC (-35)");
  (Move (PC, (encode_perm E)), "encode-decode Move PC E");
  (Move (PC, (encode_seal_perm (false, true))), "encode-decode Move PC U");
  (Move (PC, (encode_wtype W_Cap)), "encode-decode Move PC Cap");
  (Load (Reg 9, PC), "encode-decode Load R9 PC");
  (Store (PC, Register (Reg 7)), "encode-decode Store PC R7");
  (Store (PC, const (-35)), "encode-decode Store PC (-35)");
  (Store (PC, (encode_perm E)), "encode-decode Store PC E");
  (Add (Reg 5, Register (Reg 6), Register PC), "encode-decode Add R5 R6 PC");
  (Add (Reg 5, Register (Reg 6), const 8128), "encode-decode Add R5 R6 8128");
  (Add (Reg 5, Register (Reg 6), (encode_perm RO)), "encode-decode Add R5 R6 RO");
  (Add (Reg 5, const (-549), Register PC), "encode-decode Add R5 (-549) PC");
  (Add (Reg 5, const (102), const 8128), "encode-decode Add R5 102 8128");
  (Add (Reg 5, const (83), (encode_perm RO)), "encode-decode Add R5 83 RO");
  (Add (Reg 5, (encode_perm E), Register PC), "encode-decode Add R5 E PC");
  (Add (Reg 5, (encode_perm O), const 8128), "encode-decode Add R5 O 8128");
  (Add (Reg 5, (encode_perm RWX), (encode_perm RO)), "encode-decode Add R5 RWX RO");
  (Sub (Reg 5, Register (Reg 6), Register PC), "encode-decode Sub R5 R6 PC");
  (Sub (Reg 5, Register (Reg 6), const 8128), "encode-decode Sub R5 R6 8128");
  (Sub (Reg 5, Register (Reg 6), (encode_perm RO)), "encode-decode Sub R5 R6 RO");
  (Sub (Reg 5, const (-549), Register PC), "encode-decode Sub R5 (-549) PC");
  (Sub (Reg 5, const (102), const 8128), "encode-decode Sub R5 102 8128");
  (Sub (Reg 5, const (83), (encode_perm RO)), "encode-decode Sub R5 83 RO");
  (Sub (Reg 5, (encode_perm E), Register PC), "encode-decode Sub R5 E PC");
  (Sub (Reg 5, (encode_perm O), const 8128), "encode-decode Sub R5 O 8128");
  (Sub (Reg 5, (encode_perm RWX), (encode_perm RO)), "encode-decode Sub R5 RWX RO");
  (Lt (Reg 5, Register (Reg 6), Register PC), "encode-decode Lt R5 R6 PC");
  (Lt (Reg 5, Register (Reg 6), const 8128), "encode-decode Lt R5 R6 8128");
  (Lt (Reg 5, Register (Reg 6), (encode_perm RO)), "encode-decode Lt R5 R6 RO");
  (Lt (Reg 5, const (-549), Register PC), "encode-decode Lt R5 (-549) PC");
  (Lt (Reg 5, const (102), const 8128), "encode-decode Lt R5 102 8128");
  (Lt (Reg 5, const (83), (encode_perm RO)), "encode-decode Lt R5 83 RO");
  (Lt (Reg 5, (encode_perm E), Register PC), "encode-decode Lt R5 E PC");
  (Lt (Reg 5, (encode_perm O), const 8128), "encode-decode Lt R5 O 8128");
  (Lt (Reg 5, (encode_perm RWX), (encode_perm RO)), "encode-decode Lt R5 RWX RO");
  (Lea (PC, Register (Reg 7)), "encode-decode Lea PC R7");
  (Lea (PC, const (-35)), "encode-decode Lea PC (-35)");
  (Lea (PC, (encode_perm E)), "encode-decode Lea PC E");
  (Restrict (PC, Register (Reg 7)), "encode-decode Restrict PC R7");
  (Restrict (PC, const (-35)), "encode-decode Restrict PC (-35)");
  (Restrict (PC, (encode_perm E)), "encode-decode Restrict PC E");
  (SubSeg (Reg 5, Register (Reg 6), Register PC), "encode-decode SubSeg R5 R6 PC");
  (SubSeg (Reg 5, Register (Reg 6), const 8128), "encode-decode SubSeg R5 R6 8128");
  (SubSeg (Reg 5, Register (Reg 6), (encode_perm RO)), "encode-decode SubSeg R5 R6 RO");
  (SubSeg (Reg 5, const (-549), Register PC), "encode-decode SubSeg R5 (-549) PC");
  (SubSeg (Reg 5, const (102), const 8128), "encode-decode SubSeg R5 102 8128");
  (SubSeg (Reg 5, const (83), (encode_perm RO)), "encode-decode SubSeg R5 83 RO");
  (SubSeg (Reg 5, (encode_perm E), Register PC), "encode-decode SubSeg R5 E PC");
  (SubSeg (Reg 5, (encode_perm O), const 8128), "encode-decode SubSeg R5 O 8128");
  (SubSeg (Reg 5, (encode_perm RWX), (encode_perm RO)), "encode-decode SubSeg R5 RWX RO");
  (GetB (Reg 6, Reg 31), "encode-decode GetB R6 R31");
  (GetE (Reg 7, Reg 30), "encode-decode GetE R7 R30");
  (GetA (Reg 8, Reg 29), "encode-decode GetA R8 R29");
  (GetP (Reg 9, Reg 28), "encode-decode GetP R9 R28");
  (GetOType (Reg 10, Reg 27), "encode-decode GetOType R10 R27");
  (GetWType (Reg 11, Reg 26), "encode-decode GetWType R11 R26");
  (Seal (Reg 12, Reg 25, Reg 14), "encode-decode Seal R12 R25 R15");
  (UnSeal (Reg 13, Reg 24, Reg 15), "encode-decode UnSeal R13 R24 R14");
  (Fail, "encode-decode Fail");
  (Halt, "encode-decode Halt");
]

let () =
  let open Alcotest in
  run "Lib" [
    "Lex/Pars",
      List.map make_op_test instr_tests;
    "Encode/Decode Integer", 
      (test_case "interleave int" `Quick test_encode_interleave)::
      (test_case "encode pos/pos int" `Quick test_encode_pos_pos_int)::
      (test_case "encode pos/neg int" `Quick test_encode_pos_neg_int)::
      (test_case "encode neg/pos int" `Quick test_encode_neg_pos_int)::
      (test_case "encode neg/neg int" `Quick test_encode_neg_neg_int)::
      (test_case "splitting of int" `Quick test_encode_split_int)::
      (test_case "encode-decode pos/pos int" `Quick test_encode_decode_pos_pos_int)::
      (List.map test_encode_decode_int_bulk test_int_pair_list);
    "Encode/Decode Statement",
      List.map make_enc_dec_stm_tests test_enc_dec_stm_list
  ]
