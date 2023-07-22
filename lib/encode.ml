open Ast

exception DecodeException of string

let encode_locality (g : locality) : Z.t =
  Z.of_int @@
  match g with
  | Directed   -> 0b00
  | Local    -> 0b01
  | Global   -> 0b10

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

let encode_perm (p : perm) : Z.t =
  Z.of_int @@
  match p with
  (*           ULRWX *)
  | O     -> 0b00000
  | E     -> 0b00001
  | RO    -> 0b00100
  | RX    -> 0b00101
  | RW    -> 0b00110
  | RWX   -> 0b00111
  | RWL   -> 0b01110
  | RWLX  -> 0b01111
  | URW   -> 0b10110
  | URWX  -> 0b10111
  | URWL  -> 0b11110
  | URWLX -> 0b11111

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

let encode_perm_pair (p : perm) (g : locality) : Z.t =
  let open Z in
  let encoded_g = (encode_locality g) lsl 5 in (* size of perm *)
  let encoded_p = (encode_perm p) in
  encoded_g lor encoded_p


let decode_perm_pair (i : Z.t) : (perm * locality) =
  let dec_perm_exception =
    fun _ -> raise @@ DecodeException "Error decoding permission pair: unexpected encoding" in
  let open Z in
  if (i > (of_int 0b1111111))
  then dec_perm_exception ()
  else
    let decode_g = (i land (of_int 0b1100000)) asr 5 in
    let decode_p = (i land (of_int 0b0011111)) in
    (decode_perm decode_p,
     decode_locality decode_g)

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
                      
let (~$) = Z.(~$)

let encode_machine_op (s : machine_op): Z.t =
  let (^!) opcode args = Z.(opcode + (args lsl 8)) in
  let const_convert opcode c = begin
    match c with
    | Register r -> opcode, encode_reg r
    | CP (Const i) -> Z.(succ opcode, i)
    | CP (Perm (p,g)) -> Z.(succ @@ succ opcode, encode_perm_pair p g)
  end in
  let two_const_convert opcode c1 c2 = begin
    let (opc1, c1_enc) = begin
      match c1 with
      | Register r -> opcode, encode_reg r
      | CP (Const i) -> Z.(opcode + ~$3, i)
      | CP (Perm (p,g)) -> Z.(opcode + ~$6, encode_perm_pair p g)
    end in
    let (opc2, c2_enc) = const_convert opc1 c2 in
    (opc2, encode_int_int c1_enc c2_enc)
  end in
  match s with
  | Jmp r -> ~$0x00 ^! (encode_reg r)
  | Jnz (r1, r2) -> ~$0x01 ^! encode_int_int (encode_reg r1) (encode_reg r2)
  | Move (r, c) -> begin (* 0x02, 0x03, 0x04 *)
      let (opc, c_enc) = const_convert ~$0x02 c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Load (r1, r2) -> ~$0x05 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | Store (r, c) -> begin (* 0x06, 0x07, 0x08 *)
      let (opc, c_enc) = const_convert ~$0x06 c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Add (r, c1, c2) -> begin (* 0x09 to 0x11 *)
      let (opc, c_enc) = two_const_convert ~$0x09 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Sub (r, c1, c2) -> begin (* 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a *)
      let (opc, c_enc) = two_const_convert ~$0x12 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end

  | Mul (r, c1, c2) -> begin (* 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23 *)
      let (opc, c_enc) = two_const_convert ~$0x1b c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Rem (r, c1, c2) -> begin (* 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c *)
      let (opc, c_enc) = two_const_convert ~$0x24 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Div (r, c1, c2) -> begin (* 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35 *)
      let (opc, c_enc) = two_const_convert ~$0x2d c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end

  | Lt (r, c1, c2) -> begin (* 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e *)
      let (opc, c_enc) = two_const_convert ~$0x36 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Lea (r, c) -> begin (* 0x3f, 0x40, 0x41 *)
      let (opc, c_enc) = const_convert ~$0x3f c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | Restrict (r, c) -> begin (* 0x42, 0x43, 0x44 *)
      let (opc, c_enc) = const_convert ~$0x42 c in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | SubSeg (r, c1, c2) -> begin (* 0x45 to 0x4d *)
      let (opc, c_enc) = two_const_convert ~$0x45 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | IsPtr (r1, r2) -> ~$0x4e ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetL (r1, r2) -> ~$0x4f ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetP (r1, r2) -> ~$0x50 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetB (r1, r2) -> ~$0x51 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetE (r1, r2) -> ~$0x52 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | GetA (r1, r2) -> ~$0x53 ^! (encode_int_int (encode_reg r1) (encode_reg r2))
  | LoadU (r1, r2, c) -> begin (* 0x54, 0x55, 0x56 *)
      let (opc, c_enc) = const_convert ~$0x54 c in
      opc ^! (encode_int_int (encode_int_int (encode_reg r1) (encode_reg r2)) c_enc)
    end
  | StoreU (r, c1, c2) -> begin (* 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f *)
      let (opc, c_enc) = two_const_convert ~$0x57 c1 c2 in
      opc ^! (encode_int_int (encode_reg r) c_enc)
    end
  | PromoteU r -> ~$0x60 ^! (encode_reg r)
  | Fail -> ~$0x61
  | Halt -> ~$0x62

let decode_machine_op (i : Z.t) : machine_op =
  let dec_perm =
    fun c_enc -> let (p,g) = (decode_perm_pair c_enc) in Perm (p,g)
  in
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
    Move (r, CP c)
  end else
  if opc = ~$0x04 (* register perm *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let p = dec_perm c_enc in
    Move (r, CP p)
  end else
  (* Load *)
  if opc = ~$0x05
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    Load (r1, r2)
  end else
  (* Store *)
  if opc = ~$0x06 (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Store (r1, r2)
  end else
  if opc = ~$0x07 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Store (r, CP c)
  end else
  if opc = ~$0x08 (* register perm *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let p = dec_perm c_enc in
    Store (r, CP p)
  end else
  (* Add *)
  if ~$0x08 < opc && opc < ~$0x0c
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x09 then Register (decode_reg c2_enc) else
      if opc = ~$0x0a then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Add (r, c1, c2)
  end else
  if ~$0x0b < opc && opc < ~$0x0f
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x0c then Register (decode_reg c2_enc) else
      if opc = ~$0x0d then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Add (r, c1, c2)
  end else
  if ~$0x0e < opc && opc < ~$0x12
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x0f then Register (decode_reg c2_enc) else
      if opc = ~$0x10 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Add (r, c1, c2)
  end else
  (* Sub *)
  if ~$0x11 < opc && opc < ~$0x15
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x12 then Register (decode_reg c2_enc) else
      if opc = ~$0x13 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Sub (r, c1, c2)
  end else
  if ~$0x14 < opc && opc < ~$0x18
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x15 then Register (decode_reg c2_enc) else
      if opc = ~$0x16 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Sub (r, c1, c2)
  end else
  if ~$0x17 < opc && opc < ~$0x1b
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x18 then Register (decode_reg c2_enc) else
      if opc = ~$0x19 then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Sub (r, c1, c2)
  end else

  (* Mul *)
  if ~$0x1a < opc && opc < ~$0x1e
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x1b then Register (decode_reg c2_enc) else
      if opc = ~$0x1c then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Mul (r, c1, c2)
  end else
  if ~$0x1d < opc && opc < ~$0x21
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x1e then Register (decode_reg c2_enc) else
      if opc = ~$0x1f then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Mul (r, c1, c2)
  end else
  if ~$0x20 < opc && opc < ~$0x24
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x21 then Register (decode_reg c2_enc) else
      if opc = ~$0x22 then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Mul (r, c1, c2)
  end else

  (* Rem *)
  if ~$0x23 < opc && opc < ~$0x27
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x24 then Register (decode_reg c2_enc) else
      if opc = ~$0x25 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Rem (r, c1, c2)
  end else
  if ~$0x26 < opc && opc < ~$0x2a
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x27 then Register (decode_reg c2_enc) else
      if opc = ~$0x28 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Rem (r, c1, c2)
  end else
  if ~$0x29 < opc && opc < ~$0x2d
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x2a then Register (decode_reg c2_enc) else
      if opc = ~$0x2b then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Rem (r, c1, c2)
  end
  else
  (* Div *)
  if ~$0x2c < opc && opc < ~$0x30
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x2d then Register (decode_reg c2_enc) else
      if opc = ~$0x2e then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Div (r, c1, c2)
  end else
  if ~$0x2f < opc && opc < ~$0x33
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x30 then Register (decode_reg c2_enc) else
      if opc = ~$0x31 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    Div (r, c1, c2)
  end else
  if ~$0x32 < opc && opc < ~$0x36
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x33 then Register (decode_reg c2_enc) else
      if opc = ~$0x34 then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Div (r, c1, c2)
  end
  else

 (* Lt *)
  if ~$0x35 < opc && opc < ~$0x39
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x36 then Register (decode_reg c2_enc) else
      if opc = ~$0x37 then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Lt (r, c1, c2)
  end else
  if ~$0x37 < opc && opc < ~$0x3c
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x39 then Register (decode_reg c2_enc) else
      if opc = ~$0x3a then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Lt (r, c1, c2)
  end else
  if ~$0x3b < opc && opc < ~$0x3f
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x3c then Register (decode_reg c2_enc) else
      if opc = ~$0x3d then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    Lt (r, c1, c2)
  end else
  (* Lea *)
  if opc = ~$0x3f (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Lea (r1, r2)
  end else
  if opc = ~$0x40 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Lea (r, CP c)
  end else
  if opc = ~$0x41 (* register perm *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let p = dec_perm c_enc in
    Lea (r, CP p)
  end else
  (* Restrict *)
  if opc = ~$0x42 (* register register *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r1 = decode_reg r_enc in
    let r2 = Register (decode_reg c_enc) in
    Restrict (r1, r2)
  end else
  if opc = ~$0x43 (* register const *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let c = Const c_enc in
    Restrict (r, CP c)
  end else
  if opc = ~$0x44 (* register perm *)
  then begin
    let (r_enc, c_enc) = decode_int payload in
    let r = decode_reg r_enc in
    let p = dec_perm c_enc in
    Restrict (r, CP p)
  end else
  (* SubSeg *)
  if ~$0x44 < opc && opc < ~$0x48
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x45 then Register (decode_reg c2_enc) else
      if opc = ~$0x46 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    SubSeg (r, c1, c2)
  end else
  if ~$0x47 < opc && opc < ~$0x4b
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x48 then Register (decode_reg c2_enc) else
      if opc = ~$0x49 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    SubSeg (r, c1, c2)
  end else
  if ~$0x4a < opc && opc < ~$0x4e
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x4b then Register (decode_reg c2_enc) else
      if opc = ~$0x4c then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    SubSeg (r, c1, c2)
  end else  (* IsPtr *)
  if opc = ~$0x4e
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    IsPtr (r1, r2)
  end else
(* GetL *)
  if opc = ~$0x4f
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetL (r1, r2)
  end else
  (* GetP *)
  if opc = ~$0x50
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetP (r1, r2)
  end else
  (* GetB *)
  if opc = ~$0x51
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetB (r1, r2)
  end else
  (* GetE *)
  if opc = ~$0x52
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetE (r1, r2)
  end else
  (* GetA *)
  if opc = ~$0x53
  then begin
    let (r1_enc, r2_enc) = decode_int payload in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    GetA (r1, r2)
  end else

(* LoadU *)
  if opc = ~$0x54 (* register register register *)
  then begin
    let (payload', c_enc) = decode_int payload in
    let (r1_enc, r2_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = Register (decode_reg c_enc) in
    LoadU (r1, r2, c)
  end else
  if opc = ~$0x55 (* register register const *)
  then begin
    let (payload', c_enc) = decode_int payload in
    let (r1_enc, r2_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = Const c_enc in
    LoadU (r1, r2, CP c)
  end else
  if opc = ~$0x56 (* register register perm *)
  then begin
    let (payload', c_enc) = decode_int payload in
    let (r1_enc, r2_enc) = decode_int payload' in
    let r1 = decode_reg r1_enc in
    let r2 = decode_reg r2_enc in
    let c = dec_perm c_enc in
    LoadU (r1, r2, CP c)
  end else

  (* StoreU *)
  if ~$0x56 < opc && opc < ~$0x5a
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = Register (decode_reg c1_enc) in
    let c2 = begin
      if opc = ~$0x57 then Register (decode_reg c2_enc) else
      if opc = ~$0x58 then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    StoreU (r, c1, c2)
  end else
  if ~$0x59 < opc && opc < ~$0x5d
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (Const c1_enc) in
    let c2 = begin
      if opc = ~$0x5a then Register (decode_reg c2_enc) else
      if opc = ~$0x5b then CP (Const c2_enc) else
      CP (dec_perm c2_enc)
    end in
    StoreU (r, c1, c2)
  end else
  if ~$0x5c < opc && opc < ~$0x60
  then begin
    let (r_enc, payload') = decode_int payload in
    let (c1_enc, c2_enc) = decode_int payload' in
    let r = decode_reg r_enc in
    let c1 = CP (dec_perm c1_enc) in
    let c2 = begin
      if opc = ~$0x5d then Register (decode_reg c2_enc) else
      if opc = ~$0x5e then CP (Const c2_enc) else
        CP (dec_perm c2_enc)
    end in
    StoreU (r, c1, c2)
  end else

  (* PromoteU *)
  if opc = ~$0x60
  then PromoteU (decode_reg payload)
  else
  (* Fail *)
  if opc = ~$0x61
  then Fail
  else
  (* Halt *)
  if opc = ~$0x62
  then Halt
  else raise @@
    DecodeException
      (Printf.sprintf "Error decoding instruction: unrecognized opcode %0x"
         (Z.to_int opc))
