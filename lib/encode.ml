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

let locality_encoding (g : locality) : Z.t =
  Z.of_int @@ match g with Directed -> 0b00 | Local -> 0b01 | Global -> 0b10

let encode_locality (g : locality) : Z.t = encode_const _LOCALITY_ENC (locality_encoding g)

let locality_decoding (i : Z.t) : locality =
  let dec_loc_exception _ =
    raise @@ DecodeException "Error decoding locality: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  if Z.(i > of_int 0b11) then dec_loc_exception ()
  else
    match (b1, b0) with
    | false, false -> Directed
    | false, true -> Local
    | true, false -> Global
    | _ -> dec_loc_exception ()

(** Permission *)
let decode_locality (i : Z.t) : locality =
  let dec_loc_exception _ =
    raise @@ DecodeException "Error decoding locality: does not recognize a locality"
  in
  let dec_type, dec_i = decode_const i in
  if dec_type != _LOCALITY_ENC then dec_loc_exception () else locality_decoding dec_i

let perm_encoding (p : perm) : Z.t =
  Z.of_int
  @@
  match p with
  | O -> 0b00000
  | E -> 0b00001
  | RO -> 0b00100
  | RX -> 0b00101
  | RW -> 0b00110
  | RWX -> 0b00111
  | RWL -> 0b01110
  | RWLX -> 0b01111
  | URW -> 0b10110
  | URWX -> 0b10111
  | URWL -> 0b11110
  | URWLX -> 0b11111

let perm_decoding (i : Z.t) : perm =
  let dec_perm_exception _ =
    raise @@ DecodeException "Error decoding permission: unexpected encoding"
  in
  let b0 = Z.testbit i 0 in
  let b1 = Z.testbit i 1 in
  let b2 = Z.testbit i 2 in
  let b3 = Z.testbit i 3 in
  let b4 = Z.testbit i 4 in
  if Z.(i > of_int 0b11111) then dec_perm_exception ()
  else
    match (b4, b3, b2, b1, b0) with
    | false, false, false, false, false -> O
    | false, false, false, false, true -> E
    | false, false, true, false, false -> RO
    | false, false, true, false, true -> RX
    | false, false, true, true, false -> RW
    | false, false, true, true, true -> RWX
    | false, true, true, true, false -> RWL
    | false, true, true, true, true -> RWLX
    | true, false, true, true, false -> URW
    | true, false, true, true, true -> URWX
    | true, true, true, true, false -> URWL
    | true, true, true, true, true -> URWLX
    | _ -> dec_perm_exception ()

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
  let size_perm = 5 in
  let open Z in
  let encoded_g = locality_encoding g lsl size_perm in
  let encoded_p = perm_encoding p in
  encoded_g lor encoded_p

let encode_perm_loc_pair (p : perm) (g : locality) : Z.t =
  encode_const _PERM_LOC_ENC (perm_loc_pair_encoding p g)

let perm_loc_pair_decoding (i : Z.t) : perm * locality =
  let size_perm = 5 in
  let dec_perm_loc_exception _ =
    raise @@ DecodeException "Error decoding permission-locality pair: unexpected encoding"
  in
  let open Z in
  if i > of_int 0b1111111 then dec_perm_loc_exception ()
  else
    let encoded_g = (i land of_int 0b1100000) asr size_perm in
    let encoded_p = i land of_int 0b0011111 in
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
  | Jmp r -> ~$0x00 ^! encode_reg r
  | Jnz (r1, r2) -> ~$0x01 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Move (r, c) ->
      (* 0x02, 0x03 *)
      let opc, c_enc = const_convert ~$0x02 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Load (r1, r2) -> ~$0x04 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Store (r, c) ->
      (* 0x05, 0x06  *)
      let opc, c_enc = const_convert ~$0x05 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Add (r, c1, c2) ->
      (* 0x07, 0x08, 0x09, 0x0a *)
      let opc, c_enc = two_const_convert ~$0x07 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Sub (r, c1, c2) ->
      (* 0x0b, 0x0c, 0x0d, 0x0e *)
      let opc, c_enc = two_const_convert ~$0x0b c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Mul (r, c1, c2) ->
      (* 0x0f, 0x10, 0x11, 0x12 *)
      let opc, c_enc = two_const_convert ~$0x0f c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Rem (r, c1, c2) ->
      (* 0x13, 0x14, 0x15, 0x16 *)
      let opc, c_enc = two_const_convert ~$0x13 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Div (r, c1, c2) ->
      (* 0x17, 0x18, 0x19, 0x1a *)
      let opc, c_enc = two_const_convert ~$0x17 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Lt (r, c1, c2) ->
      (* 0x1b, 0x1c, 0x1d, 0x1e *)
      let opc, c_enc = two_const_convert ~$0x1b c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Lea (r, c) ->
      (* 0x1f, 0x20 *)
      let opc, c_enc = const_convert ~$0x1f c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | Restrict (r, c) ->
      (* 0x21, 0x22 *)
      let opc, c_enc = const_convert ~$0x21 c in
      opc ^! encode_int_int (encode_reg r) c_enc
  | SubSeg (r, c1, c2) ->
      (* 0x23, 0x24, 0x25, 0x26 *)
      let opc, c_enc = two_const_convert ~$0x23 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | GetL (r1, r2) -> ~$0x27 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetB (r1, r2) -> ~$0x28 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetE (r1, r2) -> ~$0x29 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetA (r1, r2) -> ~$0x2a ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetP (r1, r2) -> ~$0x2b ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetOType (r1, r2) -> ~$0x2c ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | GetWType (r1, r2) -> ~$0x2d ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Seal (r1, r2, r3) ->
      ~$0x2e ^! encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3))
  | UnSeal (r1, r2, r3) ->
      ~$0x2f ^! encode_int_int (encode_reg r1) (encode_int_int (encode_reg r2) (encode_reg r3))
  | Invoke (r1, r2) -> ~$0x30 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | LoadU (r1, r2, c) ->
      (* 0x31, 0x32 *)
      let opc, c_enc = const_convert ~$0x31 c in
      opc ^! encode_int_int (encode_int_int (encode_reg r1) (encode_reg r2)) c_enc
  | StoreU (r, c1, c2) ->
      (* 0x33, 0x34, 0x35, 0x36 *)
      let opc, c_enc = two_const_convert ~$0x33 c1 c2 in
      opc ^! encode_int_int (encode_reg r) c_enc
  | PromoteU r -> ~$0x37 ^! encode_reg r
  | EInit (r1, r2) -> ~$0x38 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | EDeInit (r1, r2) -> ~$0x39 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | EStoreId (r1, r2) -> ~$0x3a ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | IsUnique (r1, r2) -> ~$0x3b ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Fail -> ~$0x3c
  | Halt -> ~$0x3d

let decode_machine_op (i : Z.t) : machine_op =
  (* let dec_perm = *)
  (*   fun c_enc -> let (p,g) = (decode_perm_pair c_enc) in Perm (p,g) *)
  (* in *)
  let opc = Z.extract i 0 8 in
  let payload = Z.(i asr 8) in
  (* Jmp *)
  if opc = ~$0x00 then Jmp (decode_reg payload)
  else if (* Jnz *)
          opc = ~$0x01 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Jnz (r1, r2)
  else if (* Move *)
          opc = ~$0x02 (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Move (r1, r2)
  else if opc = ~$0x03 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Move (r, c)
  else if (* Load *)
          opc = ~$0x04 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Load (r1, r2)
  else if (* Store *)
          opc = ~$0x05 (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Store (r1, r2)
  else if opc = ~$0x06 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Store (r, c)
  else if (* Add *)
          ~$0x07 <= opc && opc <= ~$0x0a then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x07 || opc = ~$0x08 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x07 || opc = ~$0x09 then Register (decode_reg c2_enc) else Const c2_enc in
    Add (r, c1, c2)
  else if (* Sub *)
          ~$0x0b <= opc && opc <= ~$0x0e then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x0b || opc = ~$0x0c then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x0b || opc = ~$0x0d then Register (decode_reg c2_enc) else Const c2_enc in
    Sub (r, c1, c2)
  else if (* Mul *)
          ~$0x0f <= opc && opc <= ~$0x12 then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x0f || opc = ~$0x10 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x0f || opc = ~$0x11 then Register (decode_reg c2_enc) else Const c2_enc in
    Mul (r, c1, c2)
  else if (* Rem *)
          ~$0x13 <= opc && opc <= ~$0x16 then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x13 || opc = ~$0x14 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x13 || opc = ~$0x15 then Register (decode_reg c2_enc) else Const c2_enc in
    Rem (r, c1, c2)
  else if (* Div *)
          ~$0x17 <= opc && opc <= ~$0x1a then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x17 || opc = ~$0x18 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x17 || opc = ~$0x19 then Register (decode_reg c2_enc) else Const c2_enc in
    Div (r, c1, c2)
  else if (* Lt *)
          ~$0x1b <= opc && opc <= ~$0x1e then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x1b || opc = ~$0x1c then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x1b || opc = ~$0x1d then Register (decode_reg c2_enc) else Const c2_enc in
    Lt (r, c1, c2)
  else if (* Lea *)
          opc = ~$0x1f (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Lea (r1, r2)
  else if opc = ~$0x20 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Lea (r, c)
  else if (* Restrict *)
          opc = ~$0x21 (* register register *) then
    let r_enc, c_enc = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Restrict (r1, r2)
  else if opc = ~$0x22 (* register const *) then
    let r_enc, c_enc = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Restrict (r, c)
  else if (* Subseg *)
          ~$0x23 <= opc && opc <= ~$0x26 then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x23 || opc = ~$0x24 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x23 || opc = ~$0x25 then Register (decode_reg c2_enc) else Const c2_enc in
    SubSeg (r, c1, c2)
  else if (* GetL *)
          opc = ~$0x27 && Parameters.locality_allowed Local then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetL (r1, r2)
  else if (* GetB *)
          opc = ~$0x28 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetB (r1, r2)
  else if (* GetE *)
          opc = ~$0x29 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetE (r1, r2)
  else if (* GetA *)
          opc = ~$0x2a then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetA (r1, r2)
  else if (* GetP *)
          opc = ~$0x2b then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetP (r1, r2)
  else if (* GetOType *)
          opc = ~$0x2c && !Parameters.flags.sealing then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetOType (r1, r2)
  else if (* GetWType *)
          opc = ~$0x2d then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetWType (r1, r2)
  else if (* Seal *)
          opc = ~$0x2e && !Parameters.flags.sealing then
    let r1_enc, payload' = decode_int payload in
    let r2_enc, r3_enc = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    Seal (r1, r2, r3)
  else if (* UnSeal *)
          opc = ~$0x2f && !Parameters.flags.sealing then
    let r1_enc, payload' = decode_int payload in
    let r2_enc, r3_enc = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let r3 = decode_reg r3_enc in
    UnSeal (r1, r2, r3)
  else if (* Invoke *)
          opc = ~$0x30 && !Parameters.flags.sealing then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Invoke (r1, r2)
  else if (* LoadU *)
          opc = ~$0x31 && !Parameters.flags.unitialized (* register register register *)
  then
    let payload', c_enc = decode_int payload in
    let r1_enc, r2_enc = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = Register (decode_reg c_enc) in
    LoadU (r1, r2, c)
  else if opc = ~$0x32 && !Parameters.flags.unitialized (* register register const *) then
    let payload', c_enc = decode_int payload in
    let r1_enc, r2_enc = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = Const c_enc in
    LoadU (r1, r2, c)
  else if (* StoreU *)
          ~$0x33 <= opc && opc <= ~$0x36 && !Parameters.flags.unitialized then
    let r_enc, payload' = decode_int payload in
    let c1_enc, c2_enc = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = if opc = ~$0x33 || opc = ~$0x34 then Register (decode_reg c1_enc) else Const c1_enc in
    let c2 = if opc = ~$0x33 || opc = ~$0x35 then Register (decode_reg c2_enc) else Const c2_enc in
    StoreU (r, c1, c2)
  else if (* PromoteU *)
          opc = ~$0x37 && !Parameters.flags.unitialized then PromoteU (decode_reg payload)

  else if (* EInit *)
          opc = ~$0x38 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    EInit (r1, r2)
  else if (* EDeInit *)
          opc = ~$0x39 then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    EDeInit (r1, r2)
  else if (* EStoreId *)
          opc = ~$0x3a then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    EStoreId (r1, r2)
  else if (* IsUnique *)
          opc = ~$0x3b then
    let r1_enc, r2_enc = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    IsUnique (r1, r2)

  else if (* Fail *)
          opc = ~$0x3c then Fail
  else if (* Halt *)
          opc = ~$0x3d then Halt
  else
    raise
    @@ DecodeException
         (Printf.sprintf "Error decoding instruction: unrecognized opcode %0x" (Z.to_int opc))
