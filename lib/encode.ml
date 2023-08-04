open Ast

exception DecodeException of string

let _PERM_ENC = Z.of_int 0b00
let _SEAL_PERM_ENC = Z.of_int 0b01
let _WTYPE_ENC = Z.of_int 0b10

let encode_const (t : Z.t) (c : Z.t) : Z.t =
  let open Z in
  let enc_const = c lsl 2 in (* size of _T_ENC *)
  enc_const lor t

let decode_const (i : Z.t) : (Z.t * Z.t) =
  let open Z in
    let b0 = testbit i 0 in
    let b1 = testbit i 1 in
  let t = of_int
      (match (b1,b0) with
      | (false, false) -> 0b00
      | (false, true) -> 0b01
      | (true, false) -> 0b10
      | (true, true) -> 0b11)
  in
  (t, (of_int ((to_int i) lsr 2)))

(** WType *)

let wtype_encoding (w : wtype) : Z.t =
  Z.of_int @@
  match w with
  | W_I -> 0b00
  | W_Cap -> 0b01
  | W_SealRange -> 0b10
  | W_Sealed -> 0b11

let wtype_decoding (z : Z.t) : wtype =
  let decode_wt_exception =
    fun _ -> raise @@ DecodeException "Error decoding wtype: unexpected encoding"
  in
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

let encode_wtype (w : wtype) : Z.t = encode_const _WTYPE_ENC (wtype_encoding w)

let decode_wtype (z : Z.t) : wtype =
  let decode_wt_exception =
    fun _ -> raise @@ DecodeException "Error decoding wtype: does not recognize a wtype"
  in
  let (dec_type, dec_z) = decode_const z in
  if (dec_type != _WTYPE_ENC)
  then decode_wt_exception ()
  else wtype_decoding dec_z

(** Permission *)

let perm_encoding (p : perm) : Z.t =
    Z.of_int @@
    match p with
    | O     -> 0b00000
    | E     -> 0b00001
    | RO    -> 0b00100
    | RX    -> 0b00101
    | RW    -> 0b00110
    | RWX   -> 0b00111

let perm_decoding (i : Z.t) : perm =
  let dec_perm_exception =
    fun _ -> raise @@ DecodeException "Error decoding permission: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  let b2 = Z.testbit i 2 in
  if Z.(i > (of_int 0b111))
  then dec_perm_exception ()
  else
  match (b2,b1,b0) with
  | (false, false, false) -> O
  | (false, false, true)  -> E
  | (true, false, false)  -> RO
  | (true, false, true)   -> RX
  | (true, true, false)   -> RW
  | (true, true, true)    -> RWX
  | _ -> dec_perm_exception ()

let encode_perm (p : perm) : Z.t =
  encode_const _PERM_ENC (perm_encoding p)

let decode_perm (i : Z.t) : perm =
  let dec_perm_exception =
    fun _ -> raise @@ DecodeException "Error decoding permission: does not recognize a permission"
  in
  let (dec_type, dec_i) = decode_const i in
  if (dec_type != _PERM_ENC)
  then dec_perm_exception ()
  else perm_decoding dec_i

(** Sealing Permission *)

let seal_perm_encoding (p : seal_perm) : Z.t =
  Z.of_int @@
  match p with
  | (false, false) -> 0b00
  | (false, true) -> 0b01
  | (true, false) -> 0b10
  | (true, true) -> 0b11

let seal_perm_decoding (i : Z.t) : seal_perm =
  let decode_perm_exception =
    fun _ -> raise @@ DecodeException "Error decoding sealing permission: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  if Z.(i > (of_int 0b11))
  then decode_perm_exception ()
  else (b1,b0)

let encode_seal_perm (p : seal_perm) : Z.t =
  encode_const _SEAL_PERM_ENC (seal_perm_encoding p)

let decode_seal_perm (i : Z.t) : seal_perm =
  let decode_perm_exception =
    fun _ -> raise @@ DecodeException "Error decoding sealing permission: does not recognize a sealing permission"
  in
  let (dec_type, dec_i) = decode_const i in
  if (dec_type != _SEAL_PERM_ENC)
  then decode_perm_exception ()
  else seal_perm_decoding dec_i

let encode_reg (r : regname) : Z.t =
  match r with
  | PC -> Z.zero
  | Reg i -> Z.succ @@ Z.of_int i

let decode_reg (i : Z.t) : regname =
  if i = Z.zero
  then PC
  else Reg (Z.to_int @@ Z.pred i)

let rec split_int (i : Z.t) : Z.t * Z.t =
  let open Z in
  if i = zero
  then zero, zero
  else
    let x1 = i land one in
    let y1 = (i asr 1) land one in
    let (x2, y2) = split_int (i asr 2) in
    (x1 + (x2 lsl 1), y1 + (y2 lsl 1))

(* Interleave two integers bitwise.
 * Example: x = 0b101 and y = 0b110
 * results in 0b111001. *)
let rec interleave_int (x : Z.t) (y : Z.t) : Z.t =
  let open Z in
  if x = zero && y = zero
  then zero
  else
    let x1 = x land one in
    let y1 = (y land one) lsl 1 in
    let x2 = x asr 1 in
    let y2 = y asr 1 in
    x1 + y1 + ((interleave_int x2 y2) lsl 2)

(* Encode two integers by interleaving their
 * absolute values bitwise, followed
 * by two bits representing signs.
 *)
let encode_int_int (x : Z.t) (y : Z.t) =
  let sign_bits = Z.of_int @@ begin
    match (Z.sign y, Z.sign x) with
      | (-1, -1) -> 0b11
      | (-1, (0|1))  -> 0b10
      | ((0|1), -1)  -> 0b01
      | ((0|1), (0|1)) -> 0b00
      | _ -> assert false
  end in
  let interleaved = interleave_int (Z.abs x) (Z.abs y) in
  Z.(sign_bits + (interleaved lsl 2))

let decode_int (i : Z.t) : Z.t * Z.t =
  let is_x_neg = Z.testbit i 0 in
  let is_y_neg = Z.testbit i 1 in
  let (x, y) = Z.(split_int (i asr 2)) in
  match is_x_neg, is_y_neg with
  | (true, true) -> (Z.neg x, Z.neg y)
  | (true, false) -> (Z.neg x, y)
  | (false, true) -> (x, Z.neg y)
  | (false, false) -> (x, y)

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
  | GetB (r1, r2) -> ~$0x27 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetE (r1, r2) -> ~$0x28 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetA (r1, r2) -> ~$0x29 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetP (r1, r2) -> ~$0x2a ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetOType (r1, r2) -> ~$0x2b ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetWType (r1, r2) -> ~$0x2c ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | Seal (r1, r2, r3) ->
      ~$0x2d ^! (encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3)))
  | UnSeal (r1, r2, r3) ->
      ~$0x2e ^! (encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3)))
  | Fail -> ~$0x2f
  | Halt -> ~$0x30

let decode_machine_op (i : Z.t) : machine_op =
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

  (* GetB *)
  if opc = ~$0x27
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetB (r1, r2)
  end else
  (* GetE *)
  if opc = ~$0x28
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetE (r1, r2)
  end else
  (* GetA *)
  if opc = ~$0x29
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetA (r1, r2)
  end else
(* GetP *)
  if opc = ~$0x2a
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetP (r1, r2)
  end else
(* GetOType *)
  if opc = ~$0x2b
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetOType (r1, r2)
  end else
(* GetWType *)
  if opc = ~$0x2c
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetWType (r1, r2)
  end else

  (* Seal *)
  if opc = ~$0x2d
  then begin
    let (r1_enc, payload') = decode_int payload in
    let (r2_enc, r3_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    Seal (r1, r2, r3)
  end else
  (* UnSeal *)
  if opc = ~$0x2e
  then begin
    let (r1_enc, payload') = decode_int payload in
    let (r2_enc, r3_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    UnSeal (r1, r2, r3)
  end else

 (* Fail *)
  if opc = ~$0x2f
  then Fail
  else
  (* Halt *)
  if opc = ~$0x30
  then Halt
  else raise @@
    DecodeException
      (Printf.sprintf "Error decoding instruction: unrecognized opcode %0x"
         (Z.to_int opc))
