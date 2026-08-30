(* Encode and decode handwritten Griotte instructions and tagged metadata. The
   instruction table owns opcode allocation; the helpers below own capability tags. *)

open Ast

(* Instruction encoding. *)
let register_codec =
  Instruction_codec.scalar_codec ~name:"Griotte register"
    ~encode:(function
      | PC -> Ok Z.zero
      | Reg n when n >= 0 && n <= 31 -> Ok (Z.of_int (n + 1))
      | Reg _ -> Error "register number must be between 0 and 31")
    ~decode:(fun value ->
      if Z.equal value Z.zero then Ok PC
      else if Z.fits_int value then
        let n = Z.to_int value - 1 in
        if n >= 0 && n <= 31 then Ok (Reg n) else Error "invalid register encoding"
      else Error "invalid register encoding")

let system_register_codec =
  Instruction_codec.scalar_codec ~name:"Griotte system register"
    ~encode:(function MTDC -> Ok Z.zero)
    ~decode:(fun value -> if Z.equal value Z.zero then Ok MTDC else Error "unknown system register")

let operand = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith

let from_operand (value : (register, Z.t) Instruction_codec.register_or_constant) : reg_or_const =
  match value with
  | Instruction_codec.Register register -> Register register
  | Constant z -> Constant z

let to_operand (value : reg_or_const) : (register, Z.t) Instruction_codec.register_or_constant =
  match value with
  | Register register -> Instruction_codec.Register register
  | Constant z -> Constant z

let register_operand_codec = Instruction_codec.register register_codec
let system_register_operand_codec = Instruction_codec.register system_register_codec
let value_operand_codec = operand

let register_pair_operand_codec =
  Instruction_codec.pair register_operand_codec register_operand_codec

let register_system_pair_operand_codec =
  Instruction_codec.pair register_operand_codec system_register_operand_codec

let system_register_register_pair_operand_codec =
  Instruction_codec.pair system_register_operand_codec register_operand_codec

let register_value_pair_operand_codec =
  Instruction_codec.pair register_operand_codec value_operand_codec

let register_two_values_operand_codec =
  Instruction_codec.triple register_operand_codec value_operand_codec value_operand_codec

let register_triple_operand_codec =
  Instruction_codec.triple register_operand_codec register_operand_codec register_operand_codec

let pattern (name : string) (codec : 'a Instruction_codec.operand_codec) (construct : 'a -> 'b)
    (project : 'b -> 'a option) : 'b Instruction_codec.encoding_pattern =
  Instruction_codec.encoding_pattern ~name codec ~construct ~project

let unit_pattern (name : string) (construct : 'a) (project : 'a -> bool) :
    'a Instruction_codec.encoding_pattern =
  pattern name Instruction_codec.unit
    (fun () -> construct)
    (fun x -> if project x then Some () else None)

let encoding_patterns =
  [
    pattern "Jmp-register" register_operand_codec
      (fun register -> Jmp (Register register))
      (function Jmp (Register register) -> Some register | _ -> None);
    pattern "Jmp-constant" Instruction_codec.signed_zarith
      (fun constant -> Jmp (Constant constant))
      (function Jmp (Constant constant) -> Some constant | _ -> None);
    pattern "Jnz" register_value_pair_operand_codec
      (fun (a, b) -> Jnz (a, from_operand b))
      (function Jnz (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Jalr" register_pair_operand_codec
      (fun (a, b) -> Jalr (a, b))
      (function Jalr (a, b) -> Some (a, b) | _ -> None);
    pattern "ReadSR" register_system_pair_operand_codec
      (fun (a, b) -> ReadSR (a, b))
      (function ReadSR (a, b) -> Some (a, b) | _ -> None);
    pattern "WriteSR" system_register_register_pair_operand_codec
      (fun (a, b) -> WriteSR (a, b))
      (function WriteSR (a, b) -> Some (a, b) | _ -> None);
    pattern "Move" register_value_pair_operand_codec
      (fun (a, b) -> Move (a, from_operand b))
      (function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Load" register_pair_operand_codec
      (fun (a, b) -> Load (a, b))
      (function Load (a, b) -> Some (a, b) | _ -> None);
    pattern "Store" register_value_pair_operand_codec
      (fun (a, b) -> Store (a, from_operand b))
      (function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Add" register_two_values_operand_codec
      (fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      (function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Sub" register_two_values_operand_codec
      (fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      (function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Mul" register_two_values_operand_codec
      (fun (a, b, c) -> Mul (a, from_operand b, from_operand c))
      (function Mul (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Lt" register_two_values_operand_codec
      (fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      (function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Lea" register_value_pair_operand_codec
      (fun (a, b) -> Lea (a, from_operand b))
      (function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Restrict" register_value_pair_operand_codec
      (fun (a, b) -> Restrict (a, from_operand b))
      (function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "SubSeg" register_two_values_operand_codec
      (fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      (function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "GetL" register_pair_operand_codec
      (fun (a, b) -> GetL (a, b))
      (function GetL (a, b) -> Some (a, b) | _ -> None);
    pattern "GetB" register_pair_operand_codec
      (fun (a, b) -> GetB (a, b))
      (function GetB (a, b) -> Some (a, b) | _ -> None);
    pattern "GetE" register_pair_operand_codec
      (fun (a, b) -> GetE (a, b))
      (function GetE (a, b) -> Some (a, b) | _ -> None);
    pattern "GetA" register_pair_operand_codec
      (fun (a, b) -> GetA (a, b))
      (function GetA (a, b) -> Some (a, b) | _ -> None);
    pattern "GetP" register_pair_operand_codec
      (fun (a, b) -> GetP (a, b))
      (function GetP (a, b) -> Some (a, b) | _ -> None);
    pattern "GetOType" register_pair_operand_codec
      (fun (a, b) -> GetOType (a, b))
      (function GetOType (a, b) -> Some (a, b) | _ -> None);
    pattern "GetWType" register_pair_operand_codec
      (fun (a, b) -> GetWType (a, b))
      (function GetWType (a, b) -> Some (a, b) | _ -> None);
    pattern "Seal" register_triple_operand_codec
      (fun (a, b, c) -> Seal (a, b, c))
      (function Seal (a, b, c) -> Some (a, b, c) | _ -> None);
    pattern "UnSeal" register_triple_operand_codec
      (fun (a, b, c) -> UnSeal (a, b, c))
      (function UnSeal (a, b, c) -> Some (a, b, c) | _ -> None);
    unit_pattern "Fail" Fail (function Fail -> true | _ -> false);
    unit_pattern "Halt" Halt (function Halt -> true | _ -> false);
    pattern "LAnd" register_two_values_operand_codec
      (fun (a, b, c) -> LAnd (a, from_operand b, from_operand c))
      (function LAnd (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "LOr" register_two_values_operand_codec
      (fun (a, b, c) -> LOr (a, from_operand b, from_operand c))
      (function LOr (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "LShiftL" register_two_values_operand_codec
      (fun (a, b, c) -> LShiftL (a, from_operand b, from_operand c))
      (function LShiftL (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "LShiftR" register_two_values_operand_codec
      (fun (a, b, c) -> LShiftR (a, from_operand b, from_operand c))
      (function LShiftR (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
  ]

let table =
  match Instruction_codec.compile encoding_patterns with
  | Ok table -> table
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table
let decode = Instruction_codec.decode table

(* Capability metadata is declared once and compiled into both directions. These values are part
   of the assembly and machine contract, so decoding rejects rather than normalizes malformed
   fields. *)
let metadata_scalar ~(name : string) (mappings : ('a * int) list) =
  Tagged_metadata_codec.finite_scalar ~name
    (List.map (fun (value, encoding) -> (value, Z.of_int encoding)) mappings)

let permission_scalar =
  metadata_scalar ~name:"Griotte permission"
    [
      ((Orx, Ow, DL, DRO), 0);
      ((Orx, Ow, DL, LM), 1);
      ((Orx, Ow, LG, DRO), 2);
      ((Orx, Ow, LG, LM), 3);
      ((Orx, W, DL, DRO), 4);
      ((Orx, W, DL, LM), 5);
      ((Orx, W, LG, DRO), 6);
      ((Orx, W, LG, LM), 7);
      ((Orx, WL, DL, DRO), 8);
      ((Orx, WL, DL, LM), 9);
      ((Orx, WL, LG, DRO), 10);
      ((Orx, WL, LG, LM), 11);
      ((R, Ow, DL, DRO), 16);
      ((R, Ow, DL, LM), 17);
      ((R, Ow, LG, DRO), 18);
      ((R, Ow, LG, LM), 19);
      ((R, W, DL, DRO), 20);
      ((R, W, DL, LM), 21);
      ((R, W, LG, DRO), 22);
      ((R, W, LG, LM), 23);
      ((R, WL, DL, DRO), 24);
      ((R, WL, DL, LM), 25);
      ((R, WL, LG, DRO), 26);
      ((R, WL, LG, LM), 27);
      ((X, Ow, DL, DRO), 32);
      ((X, Ow, DL, LM), 33);
      ((X, Ow, LG, DRO), 34);
      ((X, Ow, LG, LM), 35);
      ((X, W, DL, DRO), 36);
      ((X, W, DL, LM), 37);
      ((X, W, LG, DRO), 38);
      ((X, W, LG, LM), 39);
      ((X, WL, DL, DRO), 40);
      ((X, WL, DL, LM), 41);
      ((X, WL, LG, DRO), 42);
      ((X, WL, LG, LM), 43);
      ((XSR, Ow, DL, DRO), 48);
      ((XSR, Ow, DL, LM), 49);
      ((XSR, Ow, LG, DRO), 50);
      ((XSR, Ow, LG, LM), 51);
      ((XSR, W, DL, DRO), 52);
      ((XSR, W, DL, LM), 53);
      ((XSR, W, LG, DRO), 54);
      ((XSR, W, LG, LM), 55);
      ((XSR, WL, DL, DRO), 56);
      ((XSR, WL, DL, LM), 57);
      ((XSR, WL, LG, DRO), 58);
      ((XSR, WL, LG, LM), 59);
    ]

let seal_permission_scalar =
  metadata_scalar ~name:"Griotte seal permission"
    [ ((false, false), 0); ((false, true), 1); ((true, false), 2); ((true, true), 3) ]

let locality_scalar = metadata_scalar ~name:"Griotte locality" [ (Local, 0); (Global, 1) ]

let word_type_scalar =
  metadata_scalar ~name:"Griotte word type"
    [ (W_I, 0); (W_Cap, 1); (W_SealRange, 2); (W_Sealed, 3); (W_Sentry, 4) ]

let permission_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"permission" ~tag:0
    ~wrong_tag_error:"not a Griotte permission encoding"
    ~malformed_payload_error:"unknown Griotte permission"
    (Tagged_metadata_codec.scalar_payload permission_scalar)

let seal_permission_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"seal permission" ~tag:1
    ~wrong_tag_error:"not a Griotte seal permission encoding"
    ~malformed_payload_error:"unknown Griotte seal permission"
    (Tagged_metadata_codec.scalar_payload seal_permission_scalar)

let locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"locality" ~tag:2
    ~wrong_tag_error:"not a Griotte locality encoding"
    ~malformed_payload_error:"unknown Griotte locality"
    (Tagged_metadata_codec.scalar_payload locality_scalar)

let word_type_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"word type" ~tag:3
    ~wrong_tag_error:"not a Griotte word type encoding"
    ~malformed_payload_error:"unknown Griotte word type"
    (Tagged_metadata_codec.scalar_payload word_type_scalar)

let permission_locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"permission/locality" ~tag:4
    ~wrong_tag_error:"not a Griotte permission/locality encoding"
    ~malformed_payload_error:"unknown Griotte permission/locality"
    (Tagged_metadata_codec.packed_pair ~low_width:6 ~high_width:1 permission_scalar locality_scalar)

let seal_permission_locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"seal permission/locality" ~tag:5
    ~wrong_tag_error:"not a Griotte seal permission/locality encoding"
    ~malformed_payload_error:"unknown Griotte seal permission/locality"
    (Tagged_metadata_codec.packed_pair ~low_width:2 ~high_width:1 seal_permission_scalar
       locality_scalar)

let metadata_layout =
  match
    Tagged_metadata_codec.compile
      [
        Tagged_metadata_codec.pattern permission_pattern;
        Tagged_metadata_codec.pattern seal_permission_pattern;
        Tagged_metadata_codec.pattern locality_pattern;
        Tagged_metadata_codec.pattern word_type_pattern;
        Tagged_metadata_codec.pattern permission_locality_pattern;
        Tagged_metadata_codec.pattern seal_permission_locality_pattern;
      ]
  with
  | Ok layout -> layout
  | Error errors ->
      failwith (String.concat "; " (List.map Tagged_metadata_codec.error_message errors))

let encode_metadata (pattern : 'a Tagged_metadata_codec.encoding_pattern) (value : 'a) : Z.t =
  match Tagged_metadata_codec.encode metadata_layout pattern value with
  | Ok encoded -> encoded
  | Error message -> failwith message

let encode_permission
    (permission :
      rx_permission * write_permission * deep_local_permission * deep_read_only_permission) : Z.t =
  encode_metadata permission_pattern permission

let decode_permission (encoded : Z.t) :
    ( rx_permission * write_permission * deep_local_permission * deep_read_only_permission,
      string )
    result =
  Tagged_metadata_codec.decode metadata_layout permission_pattern encoded

let encode_seal_permission (permission : bool * bool) : Z.t =
  encode_metadata seal_permission_pattern permission

let decode_seal_permission (encoded : Z.t) : (bool * bool, string) result =
  Tagged_metadata_codec.decode metadata_layout seal_permission_pattern encoded

let encode_locality (locality : locality) : Z.t = encode_metadata locality_pattern locality

let decode_locality (encoded : Z.t) : (locality, string) result =
  Tagged_metadata_codec.decode metadata_layout locality_pattern encoded

let encode_word_type (word_type : word_type) : Z.t = encode_metadata word_type_pattern word_type

let decode_word_type (encoded : Z.t) : (word_type, string) result =
  Tagged_metadata_codec.decode metadata_layout word_type_pattern encoded

let encode_permission_locality
    (permission :
      rx_permission * write_permission * deep_local_permission * deep_read_only_permission)
    (locality : locality) : Z.t =
  encode_metadata permission_locality_pattern (permission, locality)

let decode_permission_locality (encoded : Z.t) :
    ( (rx_permission * write_permission * deep_local_permission * deep_read_only_permission)
      * locality,
      string )
    result =
  Tagged_metadata_codec.decode metadata_layout permission_locality_pattern encoded

let encode_seal_permission_locality (permission : bool * bool) (locality : locality) : Z.t =
  encode_metadata seal_permission_locality_pattern (permission, locality)

let decode_seal_permission_locality (encoded : Z.t) : ((bool * bool) * locality, string) result =
  Tagged_metadata_codec.decode metadata_layout seal_permission_locality_pattern encoded
