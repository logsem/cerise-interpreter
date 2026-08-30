(** Bidirectional instruction and scalar encodings for Locality Cerise. Constructor tables are
    compiled once and shared by encoding and decoding. *)

open Ast

let register_codec =
  Instruction_codec.scalar_codec ~name:"locality-Cerise register"
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

let operand = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith

let from_operand (codec_value : (register, Z.t) Instruction_codec.register_or_constant) :
    reg_or_const =
  match codec_value with
  | Instruction_codec.Register register_encoding -> Register register_encoding
  | Instruction_codec.Constant z -> Constant z

let to_operand (codec_value : reg_or_const) : (register, Z.t) Instruction_codec.register_or_constant
    =
  match codec_value with
  | Register register_encoding -> Instruction_codec.Register register_encoding
  | Constant z -> Instruction_codec.Constant z

let register_encoding = Instruction_codec.register register_codec
let two_registers = Instruction_codec.pair register_encoding register_encoding
let register_and_operand = Instruction_codec.pair register_encoding operand
let register_and_two_operands = Instruction_codec.triple register_encoding operand operand
let three_registers = Instruction_codec.triple register_encoding register_encoding register_encoding

let unit_pattern (name : string) (construct : 'a) (project : 'a -> bool) :
    'a Instruction_codec.encoding_pattern =
  Instruction_codec.encoding_pattern ~name Instruction_codec.unit
    ~construct:(fun () -> construct)
    ~project:(fun x -> if project x then Some () else None)

let encoding_patterns =
  [
    Instruction_codec.encoding_pattern ~name:"Jmp" register_encoding
      ~construct:(fun x -> Jmp x)
      ~project:(function Jmp x -> Some x | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Jnz" two_registers
      ~construct:(fun (a, b) -> Jnz (a, b))
      ~project:(function Jnz (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Move" register_and_operand
      ~construct:(fun (a, b) -> Move (a, from_operand b))
      ~project:(function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Load" two_registers
      ~construct:(fun (a, b) -> Load (a, b))
      ~project:(function Load (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Store" register_and_operand
      ~construct:(fun (a, b) -> Store (a, from_operand b))
      ~project:(function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Add" register_and_two_operands
      ~construct:(fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      ~project:(function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Sub" register_and_two_operands
      ~construct:(fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      ~project:(function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Mul" register_and_two_operands
      ~construct:(fun (a, b, c) -> Mul (a, from_operand b, from_operand c))
      ~project:(function Mul (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Rem" register_and_two_operands
      ~construct:(fun (a, b, c) -> Rem (a, from_operand b, from_operand c))
      ~project:(function Rem (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Div" register_and_two_operands
      ~construct:(fun (a, b, c) -> Div (a, from_operand b, from_operand c))
      ~project:(function Div (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Lt" register_and_two_operands
      ~construct:(fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      ~project:(function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Lea" register_and_operand
      ~construct:(fun (a, b) -> Lea (a, from_operand b))
      ~project:(function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Restrict" register_and_operand
      ~construct:(fun (a, b) -> Restrict (a, from_operand b))
      ~project:(function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"SubSeg" register_and_two_operands
      ~construct:(fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      ~project:(function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetL" two_registers
      ~construct:(fun (a, b) -> GetL (a, b))
      ~project:(function GetL (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetB" two_registers
      ~construct:(fun (a, b) -> GetB (a, b))
      ~project:(function GetB (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetE" two_registers
      ~construct:(fun (a, b) -> GetE (a, b))
      ~project:(function GetE (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetA" two_registers
      ~construct:(fun (a, b) -> GetA (a, b))
      ~project:(function GetA (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetP" two_registers
      ~construct:(fun (a, b) -> GetP (a, b))
      ~project:(function GetP (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetOType" two_registers
      ~construct:(fun (a, b) -> GetOType (a, b))
      ~project:(function GetOType (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"GetWType" two_registers
      ~construct:(fun (a, b) -> GetWType (a, b))
      ~project:(function GetWType (a, b) -> Some (a, b) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Seal" three_registers
      ~construct:(fun (a, b, c) -> Seal (a, b, c))
      ~project:(function Seal (a, b, c) -> Some (a, b, c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"UnSeal" three_registers
      ~construct:(fun (a, b, c) -> UnSeal (a, b, c))
      ~project:(function UnSeal (a, b, c) -> Some (a, b, c) | _ -> None);
    Instruction_codec.encoding_pattern ~name:"Invoke" two_registers
      ~construct:(fun (a, b) -> Invoke (a, b))
      ~project:(function Invoke (a, b) -> Some (a, b) | _ -> None);
    unit_pattern "Fail" Fail (function Fail -> true | _ -> false);
    unit_pattern "Halt" Halt (function Halt -> true | _ -> false);
  ]

let table =
  match Instruction_codec.compile encoding_patterns with
  | Ok t -> t
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table
let decode = Instruction_codec.decode table

(* Capability metadata is declared once and compiled into both directions. *)
let metadata_scalar ~(name : string) (mappings : ('a * int) list) =
  Tagged_metadata_codec.finite_scalar ~name
    (List.map (fun (value, encoding) -> (value, Z.of_int encoding)) mappings)

let permission_scalar =
  metadata_scalar ~name:"locality-Cerise permission"
    [ (O, 0); (E, 1); (RO, 4); (RX, 5); (RW, 6); (RWX, 7); (RWL, 14); (RWLX, 15) ]

let seal_permission_scalar =
  metadata_scalar ~name:"seal permission"
    [ ((false, false), 0); ((false, true), 1); ((true, false), 2); ((true, true), 3) ]

let locality_scalar = metadata_scalar ~name:"locality" [ (Local, 1); (Global, 2) ]

let word_type_scalar =
  metadata_scalar ~name:"word type" [ (Integer, 0); (Capability, 1); (Seal_range, 2); (Sealed, 3) ]

let permission_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"permission" ~tag:0
    ~wrong_tag_error:"not a locality-Cerise permission encoding"
    ~malformed_payload_error:"unknown locality-Cerise permission"
    (Tagged_metadata_codec.scalar_payload permission_scalar)

let seal_permission_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"seal permission" ~tag:1
    ~wrong_tag_error:"not a seal permission encoding"
    ~malformed_payload_error:"unknown seal permission"
    (Tagged_metadata_codec.scalar_payload seal_permission_scalar)

let locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"locality" ~tag:2
    ~wrong_tag_error:"not a locality encoding" ~malformed_payload_error:"unknown locality"
    (Tagged_metadata_codec.scalar_payload locality_scalar)

let word_type_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"word type" ~tag:3
    ~wrong_tag_error:"not a word-type encoding" ~malformed_payload_error:"unknown word type"
    (Tagged_metadata_codec.scalar_payload word_type_scalar)

let permission_locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"permission/locality" ~tag:4
    ~wrong_tag_error:"not a permission/locality encoding"
    ~malformed_payload_error:"unknown permission/locality"
    (Tagged_metadata_codec.packed_pair ~low_width:5 ~high_width:2 permission_scalar locality_scalar)

let seal_permission_locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"seal-permission/locality" ~tag:5
    ~wrong_tag_error:"not a seal-permission/locality encoding"
    ~malformed_payload_error:"unknown seal-permission/locality"
    (Tagged_metadata_codec.packed_pair ~low_width:2 ~high_width:2 seal_permission_scalar
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

let encode_permission (permission : permission) : Z.t =
  encode_metadata permission_pattern permission

let decode_permission (encoded : Z.t) : (permission, string) result =
  Tagged_metadata_codec.decode metadata_layout permission_pattern encoded

let encode_seal_permission (permission : bool * bool) : Z.t =
  encode_metadata seal_permission_pattern permission

let decode_seal_permission (encoded : Z.t) : (bool * bool, string) result =
  Tagged_metadata_codec.decode metadata_layout seal_permission_pattern encoded

let encode_word_type (word_type : word_type) : Z.t = encode_metadata word_type_pattern word_type
let encode_locality (locality : locality) : Z.t = encode_metadata locality_pattern locality

let decode_locality (encoded : Z.t) : (locality, string) result =
  Tagged_metadata_codec.decode metadata_layout locality_pattern encoded

let encode_permission_locality (permission : permission) (locality : locality) : Z.t =
  encode_metadata permission_locality_pattern (permission, locality)

let decode_permission_locality (encoded : Z.t) : (permission * locality, string) result =
  Tagged_metadata_codec.decode metadata_layout permission_locality_pattern encoded

let encode_seal_permission_locality (permission : bool * bool) (locality : locality) : Z.t =
  encode_metadata seal_permission_locality_pattern (permission, locality)

let decode_seal_permission_locality (encoded : Z.t) : ((bool * bool) * locality, string) result =
  Tagged_metadata_codec.decode metadata_layout seal_permission_locality_pattern encoded
