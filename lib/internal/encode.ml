open Ast

exception DecodeException of string

let _PERM_ENC = Z.of_int 0b000
let _SEAL_PERM_ENC = Z.of_int 0b001
let _LOCALITY_ENC = Z.of_int 0b010
let _WTYPE_ENC = Z.of_int 0b011
let _PERM_LOC_ENC = Z.of_int 0b100
let _SEAL_LOC_ENC = Z.of_int 0b101

let encode_const (t : Z.t) (c : Z.t) : Z.t =
  let open Z in
  let enc_const = c lsl 3 in
  (* size of _T_ENC *)
  enc_const lor t

let decode_const (i : Z.t) : Z.t * Z.t =
  let open Z in
  let b0 = testbit i 0 in
  let b1 = testbit i 1 in
  let b2 = testbit i 2 in
  let t =
    of_int
      (match (b2, b1, b0) with
      | false, false, false -> 0b000
      | false, false, true -> 0b001
      | false, true, false -> 0b010
      | false, true, true -> 0b011
      | true, false, false -> 0b100
      | true, false, true -> 0b101
      | true, true, false -> 0b110
      | true, true, true -> 0b111)
  in
  (t, of_int (to_int i lsr 3))

(** WType *)

let wtype_encoding (w : wtype) : Z.t =
  Z.of_int @@ match w with W_I -> 0b00 | W_Cap -> 0b01 | W_SealRange -> 0b10 | W_Sealed -> 0b11

let wtype_decoding (z : Z.t) : wtype =
  let decode_wt_exception _ =
    raise @@ DecodeException "Error decoding wtype: unexpected encoding"
  in
  let b0 = Z.testbit z 0 in
  let b1 = Z.testbit z 1 in
  if Z.(z > of_int 0b11) then decode_wt_exception ()
  else
    match (b1, b0) with
    | false, false -> W_I
    | false, true -> W_Cap
    | true, false -> W_SealRange
    | true, true -> W_Sealed

let encode_wtype (w : wtype) : Z.t = encode_const _WTYPE_ENC (wtype_encoding w)

let decode_wtype (z : Z.t) : wtype =
  let decode_wt_exception _ =
    raise @@ DecodeException "Error decoding wtype: does not recognize a wtype"
  in
  let dec_type, dec_z = decode_const z in
  if dec_type != _WTYPE_ENC then decode_wt_exception () else wtype_decoding dec_z

(** Locality *)

let locality_encoding (g : locality) : Z.t = Z.of_int @@ match g with Local -> 0b0 | Global -> 0b1
let encode_locality (g : locality) : Z.t = encode_const _LOCALITY_ENC (locality_encoding g)

let locality_decoding (i : Z.t) : locality =
  let dec_loc_exception _ =
    raise @@ DecodeException "Error decoding locality: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  if Z.(i > of_int 0b1) then dec_loc_exception ()
  else match b0 with false -> Local | true -> Global

(** Permission *)
let decode_locality (i : Z.t) : locality =
  let dec_loc_exception _ =
    raise @@ DecodeException "Error decoding locality: does not recognize a locality"
  in
  let dec_type, dec_i = decode_const i in
  if dec_type != _LOCALITY_ENC then dec_loc_exception () else locality_decoding dec_i

let rxperm_encoding (rx : rxperm) : Z.t =
  Z.of_int @@ match rx with Orx -> 0b00 | R -> 0b01 | X -> 0b10 | XSR -> 0b11

let wperm_encoding (w : wperm) : Z.t =
  Z.of_int @@ match w with Ow -> 0b00 | W -> 0b01 | WL -> 0b10

let dlperm_encoding (dl : dlperm) : Z.t = Z.of_int @@ match dl with DL -> 0b0 | LG -> 0b1
let droperm_encoding (dro : droperm) : Z.t = Z.of_int @@ match dro with DRO -> 0b0 | LM -> 0b1

let perm_encoding (p : perm) : Z.t =
  let open Z in
  let rx, w, dl, dro = p in
  let enc_dro = droperm_encoding dro lsl 0 in
  (* size of DRO_ENCODING *)
  let enc_dl = dlperm_encoding dl lsl 1 in
  (* size of DL_ENCODING *)
  let enc_w = wperm_encoding w lsl 2 in
  (* size of W_ENCODING *)
  let enc_rx = rxperm_encoding rx lsl 4 in
  enc_rx lor enc_w lor enc_dl lor enc_dro

let perm_decoding (i : Z.t) : perm =
  let dec_perm_exception _ =
    raise @@ DecodeException "Error decoding permission: unexpected encoding"
  in
  if Z.(i > of_int 0b111111) then dec_perm_exception ()
  else
    let b k = Z.testbit i k in
    let rx =
      match (b 5, b 4) with
      | false, false -> Orx
      | false, true -> R
      | true, false -> X
      | true, true -> XSR
    in
    let w =
      match (b 3, b 2) with
      | false, false -> Ow
      | false, true -> W
      | true, false -> WL
      | true, true -> dec_perm_exception ()
    in
    let dl = match b 1 with false -> DL | true -> LG in
    let dro = match b 0 with false -> DRO | true -> LM in
    (rx, w, dl, dro)

let encode_perm (p : perm) : Z.t = encode_const _PERM_ENC (perm_encoding p)

let decode_perm (i : Z.t) : perm =
  let dec_perm_exception _ =
    raise @@ DecodeException "Error decoding permission: does not recognize a permission"
  in
  let dec_type, dec_i = decode_const i in
  if dec_type != _PERM_ENC then dec_perm_exception () else perm_decoding dec_i

(** Sealing Permission *)

let seal_perm_encoding (p : seal_perm) : Z.t =
  Z.of_int
  @@
  match p with
  | false, false -> 0b00
  | false, true -> 0b01
  | true, false -> 0b10
  | true, true -> 0b11

let seal_perm_decoding (i : Z.t) : seal_perm =
  let decode_perm_exception _ =
    raise @@ DecodeException "Error decoding sealing permission: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  if Z.(i > of_int 0b11) then decode_perm_exception () else (b1, b0)

let encode_seal_perm (p : seal_perm) : Z.t = encode_const _SEAL_PERM_ENC (seal_perm_encoding p)

let decode_seal_perm (i : Z.t) : seal_perm =
  let decode_perm_exception _ =
    raise
    @@ DecodeException "Error decoding sealing permission: does not recognize a sealing permission"
  in
  let dec_type, dec_i = decode_const i in
  if dec_type != _SEAL_PERM_ENC then decode_perm_exception () else seal_perm_decoding dec_i

(** Permission-locality encoding *)
let perm_loc_pair_encoding (p : perm) (g : locality) : Z.t =
  let size_perm = 6 in
  let open Z in
  let encoded_g = locality_encoding g lsl size_perm in
  let encoded_p = perm_encoding p in
  encoded_g lor encoded_p

let encode_perm_loc_pair (p : perm) (g : locality) : Z.t =
  encode_const _PERM_LOC_ENC (perm_loc_pair_encoding p g)

let perm_loc_pair_decoding (i : Z.t) : perm * locality =
  let size_perm = 6 in
  let dec_perm_loc_exception _ =
    raise @@ DecodeException "Error decoding permission-locality pair: unexpected encoding"
  in
  let open Z in
  if i > of_int 0b11111111 then dec_perm_loc_exception ()
  else
    let encoded_g = (i land of_int 0b11000000) asr size_perm in
    let encoded_p = i land of_int 0b00111111 in
    (perm_decoding encoded_p, locality_decoding encoded_g)

let decode_perm_loc_pair (i : Z.t) : perm * locality =
  let dec_perm_loc_exception _ =
    raise
    @@ DecodeException
         "Error decoding permission-locality pair: does not recognize a permission-locality pair"
  in
  let dec_type, dec_i = decode_const i in
  if dec_type != _PERM_LOC_ENC then dec_perm_loc_exception () else perm_loc_pair_decoding dec_i

(** Sealing Permission-locality encoding *)
let seal_perm_loc_pair_encoding (p : seal_perm) (g : locality) : Z.t =
  let size_seal_perm = 2 in
  let open Z in
  let encoded_g = locality_encoding g lsl size_seal_perm in
  let encoded_p = seal_perm_encoding p in
  encoded_g lor encoded_p

let seal_perm_loc_pair_decoding (i : Z.t) : seal_perm * locality =
  let size_seal_perm = 2 in
  let dec_perm_loc_exception _ =
    raise @@ DecodeException "Error decoding seal permission-locality pair: unexpected encoding"
  in
  let open Z in
  if i > of_int 0b1111111 then dec_perm_loc_exception ()
  else
    let encoded_g = (i land of_int 0b1100) asr size_seal_perm in
    let encoded_p = i land of_int 0b0011 in
    (seal_perm_decoding encoded_p, locality_decoding encoded_g)

let encode_seal_perm_loc_pair (p : seal_perm) (g : locality) : Z.t =
  encode_const _SEAL_LOC_ENC (seal_perm_loc_pair_encoding p g)

let decode_seal_perm_loc_pair (i : Z.t) : seal_perm * locality =
  let dec_seal_perm_loc_exception _ =
    raise
    @@ DecodeException
         "Error decoding seal permission-locality pair: does not recognize a seal \
          permission-locality pair"
  in
  let dec_type, dec_i = decode_const i in
  if dec_type != _SEAL_LOC_ENC then dec_seal_perm_loc_exception ()
  else seal_perm_loc_pair_decoding dec_i

let encode_reg (r : regname) : Z.t = match r with PC -> Z.zero | Reg i -> Z.succ @@ Z.of_int i
let decode_reg (i : Z.t) : regname = if i = Z.zero then PC else Reg (Z.to_int @@ Z.pred i)
let encode_sreg (sr : sregname) : Z.t = match sr with MTDC -> Z.zero

let decode_sreg (i : Z.t) : sregname =
  if i = Z.zero then MTDC
  else raise @@ DecodeException "Error decoding system register: unknown sregister"

let rec split_int (i : Z.t) : Z.t * Z.t =
  let open Z in
  if i = zero then (zero, zero)
  else
    let x1 = i land one in
    let y1 = (i asr 1) land one in
    let x2, y2 = split_int (i asr 2) in
    (x1 + (x2 lsl 1), y1 + (y2 lsl 1))

(* Interleave two integers bitwise.
 * Example: x = 0b101 and y = 0b110
 * results in 0b111001. *)
let rec interleave_int (x : Z.t) (y : Z.t) : Z.t =
  let open Z in
  if x = zero && y = zero then zero
  else
    let x1 = x land one in
    let y1 = (y land one) lsl 1 in
    let x2 = x asr 1 in
    let y2 = y asr 1 in
    x1 + y1 + (interleave_int x2 y2 lsl 2)

(* Encode two integers by interleaving their
 * absolute values bitwise, followed
 * by two bits representing signs.
 *)
let encode_int_int (x : Z.t) (y : Z.t) =
  let sign_bits =
    Z.of_int
    @@
    match (Z.sign y, Z.sign x) with
    | -1, -1 -> 0b11
    | -1, (0 | 1) -> 0b10
    | (0 | 1), -1 -> 0b01
    | (0 | 1), (0 | 1) -> 0b00
    | _ -> assert false
  in
  let interleaved = interleave_int (Z.abs x) (Z.abs y) in
  Z.(sign_bits + (interleaved lsl 2))

let decode_int (i : Z.t) : Z.t * Z.t =
  let is_x_neg = Z.testbit i 0 in
  let is_y_neg = Z.testbit i 1 in
  let x, y = Z.(split_int (i asr 2)) in
  match (is_x_neg, is_y_neg) with
  | true, true -> (Z.neg x, Z.neg y)
  | true, false -> (Z.neg x, y)
  | false, true -> (x, Z.neg y)
  | false, false -> (x, y)

let ( ~$ ) = Z.( ~$ )

let encode_machine_op (s : machine_op) : Z.t =
  let ( ^! ) opcode args = Z.(opcode + (args lsl 8)) in
  let const_convert opcode c =
    match c with Register r -> (opcode, encode_reg r) | Const n -> Z.(succ opcode, n)
  in
  let two_const_convert opcode c1 c2 =
    let opc1, c1_enc =
      match c1 with Register r -> (opcode, encode_reg r) | Const i -> Z.(opcode + ~$2, i)
    in
    let opc2, c2_enc = const_convert opc1 c2 in
    (opc2, encode_int_int c1_enc c2_enc)
  in
  match s with
  | Jmp c ->
      (* 0x00, 0x01 *)
      let opc, c_enc = const_convert ~$0x00 c in
      opc ^! c_enc
  | Jnz (r, c) ->
      (* 0x02, 0x03 *)
      let opc, c_enc = const_convert ~$0x02 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Jalr (r1, r2) ->
      (* 0x04 *)
      ~$0x04 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | ReadSR (r, sr) ->
      (* 0x05 *)
      ~$0x05 ^! encode_int_int (encode_reg r) (encode_sreg sr)
  | WriteSR (sr, r) ->
      (* 0x06 *)
      ~$0x06 ^! encode_int_int (encode_sreg sr) (encode_reg r)
  | Move (r, c) ->
      (* 0x07, 0x08 *)
      let opc, c_enc = const_convert ~$0x07 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Load (r1, r2) ->
      (* 0x09 *)
      ~$0x09 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Store (r, c) ->
      (* 0x0a, 0x0b  *)
      let opc, c_enc = const_convert ~$0x0a c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Add (r, c1, c2) ->
      (* 0x0c, 0x0d, 0x0e, 0x0f *)
      let opc, c_enc = two_const_convert ~$0x0c c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Sub (r, c1, c2) ->
      (* 0x10, 0x11, 0x12, 0x13 *)
      let opc, c_enc = two_const_convert ~$0x10 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Mul (r, c1, c2) ->
      (* 0x14, 0x15, 0x16, 0x17 *)
      let opc, c_enc = two_const_convert ~$0x14 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Rem (r, c1, c2) ->
      (* 0x18, 0x19, 0x1a, 0x1b *)
      let opc, c_enc = two_const_convert ~$0x18 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Div (r, c1, c2) ->
      (* 0x1c, 0x1d, 0x1e, 0x1f *)
      let opc, c_enc = two_const_convert ~$0x1c c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Lt (r, c1, c2) ->
      (* 0x20, 0x21, 0x22, 0x23 *)
      let opc, c_enc = two_const_convert ~$0x20 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Lea (r, c) ->
      (* 0x24, 0x25 *)
      let opc, c_enc = const_convert ~$0x24 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Restrict (r, c) ->
      (* 0x26, 0x27 *)
      let opc, c_enc = const_convert ~$0x26 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | SubSeg (r, c1, c2) ->
      (* 0x28, 0x29, 0x2a, 0x2b *)
      let opc, c_enc = two_const_convert ~$0x28 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | GetL (r1, r2) -> ~$0x2c ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetB (r1, r2) -> ~$0x2d ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetE (r1, r2) -> ~$0x2e ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetA (r1, r2) -> ~$0x2f ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetP (r1, r2) -> ~$0x30 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetOType (r1, r2) -> ~$0x31 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetWType (r1, r2) -> ~$0x32 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Seal (r1, r2, r3) ->
      ~$0x33 ^! encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3))
  | UnSeal (r1, r2, r3) ->
      ~$0x34 ^! encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3))
  | Fail -> ~$0x35
  | Halt -> ~$0x36

let decode_machine_op (i : Z.t) : machine_op =
  (* let dec_perm = *)
  (*   fun c_enc -> let (p,g) = (decode_perm_pair c_enc) in Perm (p,g) *)
  (* in *)
  let opc = Z.extract i 0 8 in
  let payload = Z.(i asr 8) in

  (* Jmp *)
  if opc = ~$0x00 (* register register *) then
    let r = Register (decode_reg payload) in
    Jmp r
  else if opc = ~$0x01 (* register const *) then
    let r = Const payload in
    Jmp r
  else if (* Jnz *)
          opc = ~$0x02 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = Register (decode_reg r2_enc) in
    Jnz (r1, r2)
  else if opc = ~$0x03 then
    let r1_enc, c_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let c = Const c_enc in
    Jnz (r1, c)
  else if (* Jalr *)
          opc = ~$0x04 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Jalr (r1, r2)
  else if (* ReadSR *)
          opc = ~$0x05 (* register sregister *) then
    let r_enc, sr_enc = decode_int payload in
    let r = decode_reg r_enc in
    let sr = decode_sreg sr_enc in
    ReadSR (r, sr)
  else if (* WriteSR *)
          opc = ~$0x06 (* sregister register *) then
    let sr_enc, r_enc = decode_int payload in
    let sr = decode_sreg sr_enc in
    let r = decode_reg r_enc in
    WriteSR (sr, r)
  else if (* Move *)
          opc = ~$0x07 (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Move (r1, r2)
  else if opc = ~$0x08 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Move (r, c)
  else if (* Load *)
          opc = ~$0x09 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Load (r1, r2)
  else if (* Store *)
          opc = ~$0x0a (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Store (r1, r2)
  else if opc = ~$0x0b (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Store (r, c)
  else if (* Add *)
          ~$0x0c <= opc && opc <= ~$0x0f then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x0c || opc = ~$0x0d then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x0c || opc = ~$0x0e then Register (decode_reg c2_enc) else Const c2_enc in
    Add (r, c1, c2)
  else if (* Sub *)
          ~$0x10 <= opc && opc <= ~$0x13 then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x10 || opc = ~$0x11 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x10 || opc = ~$0x12 then Register (decode_reg c2_enc) else Const c2_enc in
    Sub (r, c1, c2)
  else if (* Mul *)
          ~$0x14 <= opc && opc <= ~$0x17 then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x14 || opc = ~$0x15 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x14 || opc = ~$0x16 then Register (decode_reg c2_enc) else Const c2_enc in
    Mul (r, c1, c2)
  else if (* Rem *)
          ~$0x18 <= opc && opc <= ~$0x1b then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x18 || opc = ~$0x19 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x18 || opc = ~$0x1a then Register (decode_reg c2_enc) else Const c2_enc in
    Rem (r, c1, c2)
  else if (* Div *)
          ~$0x1c <= opc && opc <= ~$0x1f then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x1c || opc = ~$0x1d then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x1c || opc = ~$0x1e then Register (decode_reg c2_enc) else Const c2_enc in
    Div (r, c1, c2)
  else if (* Lt *)
          ~$0x20 <= opc && opc <= ~$0x23 then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x20 || opc = ~$0x21 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x20 || opc = ~$0x22 then Register (decode_reg c2_enc) else Const c2_enc in
    Lt (r, c1, c2)
  else if (* Lea *)
          opc = ~$0x24 (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Lea (r1, r2)
  else if opc = ~$0x25 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Lea (r, c)
  else if (* Restrict *)
          opc = ~$0x26 (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Restrict (r1, r2)
  else if opc = ~$0x27 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Restrict (r, c)
  else if (* Subseg *)
          ~$0x28 <= opc && opc <= ~$0x2b then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x28 || opc = ~$0x29 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x28 || opc = ~$0x2a then Register (decode_reg c2_enc) else Const c2_enc in
    SubSeg (r, c1, c2)
  else if (* GetL *)
          opc = ~$0x2c then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetL (r1, r2)
  else if (* GetB *)
          opc = ~$0x2d then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetB (r1, r2)
  else if (* GetE *)
          opc = ~$0x2e then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetE (r1, r2)
  else if (* GetA *)
          opc = ~$0x2f then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetA (r1, r2)
  else if (* GetP *)
          opc = ~$0x30 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetP (r1, r2)
  else if (* GetOType *)
          opc = ~$0x31 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetOType (r1, r2)
  else if (* GetWType *)
          opc = ~$0x32 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetWType (r1, r2)
  else if (* Seal *)
          opc = ~$0x33 then
    let r1_enc, payload' = decode_int payload in
    let r2_enc, r3_enc = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    Seal (r1, r2, r3)
  else if (* UnSeal *)
          opc = ~$0x34 then
    let r1_enc, payload' = decode_int payload in
    let r2_enc, r3_enc = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    UnSeal (r1, r2, r3)
  else if (* Fail *) opc = ~$0x35 then Fail
  else if (* Halt *) opc = ~$0x36 then Halt
  else
    raise
    @@ DecodeException
         (Printf.sprintf "Error decoding instruction: unrecognized opcode %0x" (Z.to_int opc))
