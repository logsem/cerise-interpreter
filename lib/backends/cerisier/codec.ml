(** Bidirectional instruction and scalar encodings for Cerisier. Constructor tables are compiled
    once and shared by encoding and decoding. *)

open Ast

let register_codec =
  Instruction_codec.scalar_codec ~name:"Cerisier register"
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

let pattern (name : string) (operand_codec : 'a Instruction_codec.operand_codec) :
    construct:('a -> instruction) ->
    project:(instruction -> 'a option) ->
    instruction Instruction_codec.encoding_pattern =
  Instruction_codec.encoding_pattern ~name operand_codec

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
    pattern "IsUnique" two_registers
      ~construct:(fun (a, b) -> IsUnique (a, b))
      ~project:(function IsUnique (a, b) -> Some (a, b) | _ -> None);
    pattern "Hash" two_registers
      ~construct:(fun (a, b) -> Hash (a, b))
      ~project:(function Hash (a, b) -> Some (a, b) | _ -> None);
    pattern "HashConcat" register_and_two_operands
      ~construct:(fun (a, b, c) -> HashConcat (a, from_operand b, from_operand c))
      ~project:(function HashConcat (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "EInit" two_registers
      ~construct:(fun (a, b) -> EInit (a, b))
      ~project:(function EInit (a, b) -> Some (a, b) | _ -> None);
    pattern "EDeInit" register_encoding
      ~construct:(fun a -> EDeInit a)
      ~project:(function EDeInit a -> Some a | _ -> None);
    pattern "EStoreId" two_registers
      ~construct:(fun (a, b) -> EStoreId (a, b))
      ~project:(function EStoreId (a, b) -> Some (a, b) | _ -> None);
  ]

let table =
  match Instruction_codec.compile encoding_patterns with
  | Ok t -> t
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table
let decode = Instruction_codec.decode table
let encode_tag (tag : int) (scalar : Z.t) : Z.t = Z.logor (Z.of_int tag) (Z.shift_left scalar 3)

let permission_scalar (codec_value : permission) : int =
  match codec_value with O -> 0 | E -> 1 | RO -> 4 | RX -> 5 | RW -> 6 | RWX -> 7

let encode_permission (p : permission) : Z.t = encode_tag 0 (Z.of_int (permission_scalar p))

let decode_permission (z : Z.t) : (permission, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) Z.zero) then
    Error "not a Cerisier permission encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok O
    | payload when Z.equal payload Z.one -> Ok E
    | payload when Z.equal payload (Z.of_int 4) -> Ok RO
    | payload when Z.equal payload (Z.of_int 5) -> Ok RX
    | payload when Z.equal payload (Z.of_int 6) -> Ok RW
    | payload when Z.equal payload (Z.of_int 7) -> Ok RWX
    | _ -> Error "unknown Cerisier permission"

let encode_seal_permission ((s, u) : bool * bool) : Z.t =
  encode_tag 1 (Z.of_int ((if s then 2 else 0) + if u then 1 else 0))

let decode_seal_permission (z : Z.t) : (bool * bool, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 1)) then
    Error "not a seal permission encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok (false, false)
    | payload when Z.equal payload Z.one -> Ok (false, true)
    | payload when Z.equal payload (Z.of_int 2) -> Ok (true, false)
    | payload when Z.equal payload (Z.of_int 3) -> Ok (true, true)
    | _ -> Error "unknown seal permission"

let encode_word_type (codec_value : word_type) : Z.t =
  match codec_value with
  | Integer -> encode_tag 3 Z.zero
  | Capability -> encode_tag 3 Z.one
  | Seal_range -> encode_tag 3 (Z.of_int 2)
  | Sealed -> encode_tag 3 (Z.of_int 3)

let decode_word_type (z : Z.t) : (word_type, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 3)) then
    Error "not a word-type encoding"
  else
    match Z.shift_right z 3 with
    | payload when Z.equal payload Z.zero -> Ok Integer
    | payload when Z.equal payload Z.one -> Ok Capability
    | payload when Z.equal payload (Z.of_int 2) -> Ok Seal_range
    | payload when Z.equal payload (Z.of_int 3) -> Ok Sealed
    | _ -> Error "unknown word type"
