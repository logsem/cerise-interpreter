open Ast

exception DecodeException of string

let encode_wtype (w : wtype) =
  Ir.encode_wtype
  (match w with
   | W_I -> Ir.W_I
   | W_Cap -> Ir.W_Cap
   | W_SealRange -> Ir.W_SealRange
   | W_Sealed -> Ir.W_Sealed)

let decode_wtype (z : Z.t) : wtype =
  let decode_wt_exception = fun _ -> raise @@ DecodeException "Error decoding wtype: unexpected encoding" in
  let b0 = Z.testbit z 0 in
  let b1 = Z.testbit z 1 in
  if Z.(z > (of_int 0b11))
  then decode_wt_exception ()
  else
  match (b1,b0) with
  | (false, false) -> W_I
  | (false, true) -> W_Cap
  | (true, false) -> W_SealRange
  | (true, true) -> W_Sealed

let encode_locality (p : locality) =
  Ir.encode_locality
    (match p with
     | Directed -> Ir.Directed
     | Local -> Ir.Local
     | Global -> Ir.Global)

let decode_locality (i : Z.t) : locality =
  let dec_loc_exception =
    fun _ -> raise @@ DecodeException
        "Error decoding locality: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  if Z.(i > (of_int 0b11))
  then dec_loc_exception ()
  else
  match (b1,b0) with
    | (false, false) -> Directed
    | (false, true) -> Local
    | (true, false) -> Global
    | _ -> dec_loc_exception ()


let encode_perm (p : perm) =
  Ir.encode_perm
    (match p with
     | O -> Ir.O
     | E -> Ir.E
     | RO -> Ir.RO
     | RX -> Ir.RX
     | RW -> Ir.RW
     | RWX -> Ir.RWX
     | RWL -> Ir.RWL
     | RWLX -> Ir.RWLX
     | URW -> Ir.URW
     | URWX -> Ir.URWX
     | URWL -> Ir.URWL
     | URWLX -> Ir.URWLX
    )

let decode_perm (i : Z.t) : perm =
  let dec_perm_exception =
    fun _ -> raise @@ DecodeException "Error decoding permission: unexpected encoding" in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  let b2 = Z.testbit i 2 in
  let b3 = Z.testbit i 3 in
  let b4 = Z.testbit i 4 in
  if Z.(i > (of_int 0b11111))
  then dec_perm_exception ()
  else
  match (b4,b3,b2,b1,b0) with
  | (false, false, false, false, false) -> O
  | (false, false, false, false, true)  -> E
  | (false, false, true, false, false)  -> RO
  | (false, false, true, false, true)   -> RX
  | (false, false, true, true, false)   -> RW
  | (false, false, true, true, true)    -> RWX
  | (false, true, true, true, false)    -> RWL
  | (false, true, true, true, true)     -> RWLX
  | (true, false, true, true, false)    -> URW
  | (true, false, true, true, true)     -> URWX
  | (true, true, true, true, false)     -> URWL
  | (true, true, true, true, true)      -> URWLX
  | _ -> dec_perm_exception ()

(* let encode_perm_pair (p : perm) (g : locality) : Z.t = *)
(*   let open Z in *)
(*   let encoded_g = (encode_locality g) lsl 5 in (\* size of perm *\) *)
(*   let encoded_p = (encode_perm p) in *)
(*   encoded_g lor encoded_p *)


(* let decode_perm_pair (i : Z.t) : (perm * locality) = *)
(*   let dec_perm_exception = *)
(*     fun _ -> raise @@ DecodeException "Error decoding permission pair: unexpected encoding" in *)
(*   let open Z in *)
(*   if (i > (of_int 0b1111111)) *)
(*   then dec_perm_exception () *)
(*   else *)
(*     let decode_g = (i land (of_int 0b1100000)) asr 5 in *)
(*     let decode_p = (i land (of_int 0b0011111)) in *)
(*     (decode_perm decode_p, *)
(*      decode_locality decode_g) *)

let encode_seal_perm (sp : seal_perm) : Z.t = Ir.encode_seal_perm sp

let decode_seal_perm (i : Z.t) : seal_perm =
  let decode_perm_exception = fun _ -> raise @@ DecodeException "Error decoding sealing permission: unexpected encoding" in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  if Z.(i > (of_int 0b11))
  then decode_perm_exception ()
  else (b1,b0)

let encode_reg (r : regname) : Z.t =
  match r with
  | PC -> Z.zero
  | STK -> Z.succ @@ Z.zero
  | Reg i -> Z.succ @@ Z.succ @@ Z.of_int i

let decode_reg (i : Z.t) : regname =
  if i = Z.zero
  then PC
  else
  if i = Z.succ @@ Z.zero
  then STK else
    Reg (Z.to_int @@ Z.pred @@ Z.pred i)


let rec split_int (i : Z.t) : Z.t * Z.t =
  let open Z in
  if i = zero
  then zero, zero
  else
    let x1 = i land one in
    let y1 = (i asr 1) land one in
    let (x2, y2) = split_int (i asr 2) in
    (x1 + (x2 lsl 1), y1 + (y2 lsl 1))

let decode_int (i : Z.t) : Z.t * Z.t =
  let is_x_neg = Z.testbit i 0 in
  let is_y_neg = Z.testbit i 1 in
  let (x, y) = Z.(split_int (i asr 2)) in
  match is_x_neg, is_y_neg with
  | (true, true) -> (Z.neg x, Z.neg y)
  | (true, false) -> (Z.neg x, y)
  | (false, true) -> (x, Z.neg y)
  | (false, false) -> (x, y)

let encode_int_int = Ir.encode_int_int
                      
let (~$) = Z.(~$)

let encode_machine_op (s : machine_op): Z.t =
  let (^!) opcode args = Z.(opcode + (args lsl 8)) in
  let const_convert opcode c = begin
    match c with
    | Register r -> opcode, encode_reg r
    | Const n ->  Z.(succ opcode, n)
  end in
  let two_const_convert opcode c1 c2 = begin
    let (opc1, c1_enc) = begin
      match c1 with
      | Register r -> opcode, encode_reg r
      | Const i -> Z.(opcode + ~$2, i)
    end in
    let (opc2, c2_enc) = const_convert opc1 c2 in
    (opc2, encode_int_int c1_enc c2_enc)
  end in
  match s with
  | Jmp r -> ~$0x00 ^! (encode_reg r)
  | Jnz (r1, r2) -> ~$0x01 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Move (r, c) -> begin (* 0x02, 0x03 *)
      let (opc, c_enc) = const_convert ~$0x02 c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Load (r1, r2) -> ~$0x04 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | Store (r, c) -> begin (* 0x05, 0x06  *)
      let (opc, c_enc) = const_convert ~$0x05 c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Add (r, c1, c2) -> begin (* 0x07, 0x08, 0x09, 0x0a *)
      let (opc, c_enc) = two_const_convert ~$0x07 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Sub (r, c1, c2) -> begin (* 0x0b, 0x0c, 0x0d, 0x0e *)
      let (opc, c_enc) = two_const_convert ~$0x0b c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end

  | Mul (r, c1, c2) -> begin (* 0x0f, 0x10, 0x11, 0x12 *)
      let (opc, c_enc) = two_const_convert ~$0x0f c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Rem (r, c1, c2) -> begin (* 0x13, 0x14, 0x15, 0x16 *)
      let (opc, c_enc) = two_const_convert ~$0x13 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Div (r, c1, c2) -> begin (* 0x17, 0x18, 0x19, 0x1a *)
      let (opc, c_enc) = two_const_convert ~$0x17 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end

  | Lt (r, c1, c2) -> begin (* 0x1b, 0x1c, 0x1d, 0x1e *)
      let (opc, c_enc) = two_const_convert ~$0x1b c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Lea (r, c) -> begin (* 0x1f, 0x20 *)
      let (opc, c_enc) = const_convert ~$0x1f c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Restrict (r, c) -> begin (* 0x21, 0x22 *)
      let (opc, c_enc) = const_convert ~$0x21 c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | SubSeg (r, c1, c2) -> begin (* 0x23, 0x24, 0x25, 0x26 *)
      let (opc, c_enc) = two_const_convert ~$0x23 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | GetL (r1, r2) -> ~$0x27 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetB (r1, r2) -> ~$0x28 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetE (r1, r2) -> ~$0x29 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetA (r1, r2) -> ~$0x2a ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetP (r1, r2) -> ~$0x2b ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetOType (r1, r2) -> ~$0x2c ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetWType (r1, r2) -> ~$0x2d ^! (encode_int_int (encode_reg r1) (encode_reg r2))

  | Seal (r1, r2, r3) ->
      ~$0x2e ^! (encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3)))
  | UnSeal (r1, r2, r3) ->
      ~$0x2f ^! (encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3)))
  | LoadU (r1, r2, c) -> begin (* 0x30, 0x31 *)
      let (opc, c_enc) = const_convert ~$0x30 c in
      opc ^! (encode_int_int (encode_int_int (encode_reg r1) (encode_reg r2)) c_enc)
    end
  | StoreU (r, c1, c2) -> begin (* 0x32, 0x33, 0x34, 0x35 *)
      let (opc, c_enc) = two_const_convert ~$0x32 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | PromoteU r -> ~$0x36 ^! (encode_reg r)
  | Fail -> ~$0x37
  | Halt -> ~$0x38

let decode_machine_op (i : Z.t) : machine_op =
  (* let dec_perm = *)
  (*   fun c_enc -> let (p,g) = (decode_perm_pair c_enc) in Perm (p,g) *)
  (* in *)
  let opc = Z.extract i 0 8 in
  let payload = Z.(i asr 8) in
  (* Jmp *)
  if opc = ~$0x00
  then Jmp (decode_reg payload)
  else
  (* Jnz *)
  if opc = ~$0x01
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Jnz (r1, r2)
  end else
  (* Move *)
  if opc = ~$0x02 (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Move (r1, r2)
  end else
  if opc = ~$0x03 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Move (r, c)
  end else

  (* Load *)
  if opc = ~$0x04
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Load (r1, r2)
  end else

  (* Store *)
  if opc = ~$0x05 (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Store (r1, r2)
  end else
  if opc = ~$0x06 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Store (r, c)
  end else

  (* Add *)
  if ~$0x07 <= opc && opc <= ~$0x0a
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x07 || opc = ~$0x08
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x07 || opc = ~$0x09
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    Add (r, c1, c2)
  end else

  (* Sub *)
  if ~$0x0b <= opc && opc <= ~$0x0e
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x0b || opc = ~$0x0c
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x0b || opc = ~$0x0d
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    Sub (r, c1, c2)
  end else

  (* Mul *)
  if ~$0x0f <= opc && opc <= ~$0x12
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x0f || opc = ~$0x10
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x0f || opc = ~$0x11
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    Mul (r, c1, c2)
  end else

  (* Rem *)
  if ~$0x13 <= opc && opc <= ~$0x16
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x13 || opc = ~$0x14
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x13 || opc = ~$0x15
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    Rem (r, c1, c2)
  end else

  (* Div *)
  if ~$0x17 <= opc && opc <= ~$0x1a
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x17 || opc = ~$0x18
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x17 || opc = ~$0x19
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    Div (r, c1, c2)
  end else

  (* Lt *)
  if ~$0x1b <= opc && opc <= ~$0x1e
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x1b || opc = ~$0x1c
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x1b || opc = ~$0x1d
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    Lt (r, c1, c2)
  end else

  (* Lea *)
  if opc = ~$0x1f (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Lea (r1, r2)
  end else
  if opc = ~$0x20 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Lea (r, c)
  end else

  (* Restrict *)
  if opc = ~$0x21 (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Restrict (r1, r2)
  end else
  if opc = ~$0x22 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Restrict (r, c)
  end else

  (* Subseg *)
  if ~$0x23 <= opc && opc <= ~$0x26
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x23 || opc = ~$0x24
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x23 || opc = ~$0x25
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    SubSeg (r, c1, c2)
  end else
(* GetL *)
  if opc = ~$0x27
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetL (r1, r2)
  end else

  (* GetB *)
  if opc = ~$0x28
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetB (r1, r2)
  end else
  (* GetE *)
  if opc = ~$0x29
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetE (r1, r2)
  end else
  (* GetA *)
  if opc = ~$0x2a
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetA (r1, r2)
  end else
(* GetP *)
  if opc = ~$0x2b
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetP (r1, r2)
  end else
(* GetOType *)
  if opc = ~$0x2c
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetOType (r1, r2)
  end else
(* GetWType *)
  if opc = ~$0x2d
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetWType (r1, r2)
  end else

  (* Seal *)
  if opc = ~$0x2e
  then begin
    let (r1_enc, payload') = decode_int payload in
    let (r2_enc, r3_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    Seal (r1, r2, r3)
  end else
  (* UnSeal *)
  if opc = ~$0x2f
  then begin
    let (r1_enc, payload') = decode_int payload in
    let (r2_enc, r3_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    UnSeal (r1, r2, r3)
  end else

  if opc = ~$0x30 (* register register register *)
  then begin
    let (payload', c_enc) = decode_int payload in
    let (r1_enc, r2_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = Register (decode_reg c_enc) in
    LoadU (r1, r2, c)
  end else
  if opc = ~$0x31 (* register register const *)
  then begin
    let (payload', c_enc) = decode_int payload in
    let (r1_enc, r2_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = Const c_enc in
    LoadU (r1, r2, c)
  end else
  (* StoreU *)
  if ~$0x32 <= opc && opc <= ~$0x35
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 =
      if opc = ~$0x32 || opc = ~$0x33
      then Register (decode_reg c1_enc)
      else Const c1_enc
    in
    let c2 =
      if opc = ~$0x32 || opc = ~$0x34
      then Register (decode_reg c2_enc)
      else Const c2_enc
    in
    StoreU (r, c1, c2)
  end else
  (* PromoteU *)
  if opc = ~$0x36
  then PromoteU (decode_reg payload)
  else
  (* Fail *)
  if opc = ~$0x37
  then Fail
  else
  (* Halt *)
  if opc = ~$0x38
  then Halt
  else raise @@
    DecodeException
      (Printf.sprintf "Error decoding instruction: unrecognized opcode %0x"
         (Z.to_int opc))
