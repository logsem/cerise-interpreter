(** Bidirectional instruction and capability-field encoding for mCerise. Operand codecs define
    instruction payloads once so encoding and decoding cannot silently drift apart. *)

open Ast

let register_codec =
  Instruction_codec.scalar_codec ~name:"mCerise register"
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

let operand_codec = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith

let from_operand (value : (register, Z.t) Instruction_codec.register_or_constant) : reg_or_const =
  match value with
  | Instruction_codec.Register register -> Register register
  | Constant z -> Constant z

let to_operand (value : reg_or_const) : (register, Z.t) Instruction_codec.register_or_constant =
  match value with
  | Register register -> Instruction_codec.Register register
  | Constant z -> Constant z

let register_operand_codec = Instruction_codec.register register_codec

let two_registers_operand_codec =
  Instruction_codec.pair register_operand_codec register_operand_codec

let register_and_value_operand_codec = Instruction_codec.pair register_operand_codec operand_codec

let register_and_two_values_operand_codec =
  Instruction_codec.triple register_operand_codec operand_codec operand_codec

let two_registers_and_value_operand_codec =
  Instruction_codec.triple register_operand_codec register_operand_codec operand_codec

let pattern (name : string) (codec : 'a Instruction_codec.operand_codec) (construct : 'a -> 'b)
    (project : 'b -> 'a option) : 'b Instruction_codec.encoding_pattern =
  Instruction_codec.encoding_pattern ~name codec ~construct ~project

let unit_pattern (name : string) (construct : 'a) (project : 'a -> bool) :
    'a Instruction_codec.encoding_pattern =
  pattern name Instruction_codec.unit
    (fun () -> construct)
    (fun x -> if project x then Some () else None)

(** Instruction table: operand codecs and constructor projections. *)
let encoding_patterns : instruction Instruction_codec.encoding_pattern list =
  [
    pattern "Jmp" register_operand_codec (fun x -> Jmp x) (function Jmp x -> Some x | _ -> None);
    pattern "Jnz" two_registers_operand_codec
      (fun (a, b) -> Jnz (a, b))
      (function Jnz (a, b) -> Some (a, b) | _ -> None);
    pattern "Move" register_and_value_operand_codec
      (fun (a, b) -> Move (a, from_operand b))
      (function Move (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Load" two_registers_operand_codec
      (fun (a, b) -> Load (a, b))
      (function Load (a, b) -> Some (a, b) | _ -> None);
    pattern "Store" register_and_value_operand_codec
      (fun (a, b) -> Store (a, from_operand b))
      (function Store (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Add" register_and_two_values_operand_codec
      (fun (a, b, c) -> Add (a, from_operand b, from_operand c))
      (function Add (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Sub" register_and_two_values_operand_codec
      (fun (a, b, c) -> Sub (a, from_operand b, from_operand c))
      (function Sub (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Lt" register_and_two_values_operand_codec
      (fun (a, b, c) -> Lt (a, from_operand b, from_operand c))
      (function Lt (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "Lea" register_and_value_operand_codec
      (fun (a, b) -> Lea (a, from_operand b))
      (function Lea (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "Restrict" register_and_value_operand_codec
      (fun (a, b) -> Restrict (a, from_operand b))
      (function Restrict (a, b) -> Some (a, to_operand b) | _ -> None);
    pattern "SubSeg" register_and_two_values_operand_codec
      (fun (a, b, c) -> SubSeg (a, from_operand b, from_operand c))
      (function SubSeg (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "IsPtr" two_registers_operand_codec
      (fun (a, b) -> IsPtr (a, b))
      (function IsPtr (a, b) -> Some (a, b) | _ -> None);
    pattern "GetP" two_registers_operand_codec
      (fun (a, b) -> GetP (a, b))
      (function GetP (a, b) -> Some (a, b) | _ -> None);
    pattern "GetL" two_registers_operand_codec
      (fun (a, b) -> GetL (a, b))
      (function GetL (a, b) -> Some (a, b) | _ -> None);
    pattern "GetB" two_registers_operand_codec
      (fun (a, b) -> GetB (a, b))
      (function GetB (a, b) -> Some (a, b) | _ -> None);
    pattern "GetE" two_registers_operand_codec
      (fun (a, b) -> GetE (a, b))
      (function GetE (a, b) -> Some (a, b) | _ -> None);
    pattern "GetA" two_registers_operand_codec
      (fun (a, b) -> GetA (a, b))
      (function GetA (a, b) -> Some (a, b) | _ -> None);
    unit_pattern "Fail" Fail (function Fail -> true | _ -> false);
    unit_pattern "Halt" Halt (function Halt -> true | _ -> false);
    pattern "LoadU" two_registers_and_value_operand_codec
      (fun (a, b, c) -> LoadU (a, b, from_operand c))
      (function LoadU (a, b, c) -> Some (a, b, to_operand c) | _ -> None);
    pattern "StoreU" register_and_two_values_operand_codec
      (fun (a, b, c) -> StoreU (a, from_operand b, from_operand c))
      (function StoreU (a, b, c) -> Some (a, to_operand b, to_operand c) | _ -> None);
    pattern "PromoteU" register_operand_codec
      (fun a -> PromoteU a)
      (function PromoteU a -> Some a | _ -> None);
  ]

let table =
  match Instruction_codec.compile encoding_patterns with
  | Ok table -> table
  (* Pattern declarations are static. An invalid table means the backend itself is inconsistent,
     so initialization must fail rather than choose an encoding. *)
  | Error errors -> failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode (instruction : instruction) : (Z.t, Instruction_codec.error) result =
  Instruction_codec.encode table instruction

let decode (encoded : Z.t) : (instruction, Instruction_codec.error) result =
  Instruction_codec.decode table encoded

(* Capability metadata is declared once and compiled into both directions. *)
let metadata_scalar ~(name : string) (mappings : ('a * int) list) =
  Tagged_metadata_codec.finite_scalar ~name
    (List.map (fun (value, encoding) -> (value, Z.of_int encoding)) mappings)

let permission_scalar =
  metadata_scalar ~name:"mCerise permission"
    [
      (O, 0);
      (E, 1);
      (RO, 4);
      (RX, 5);
      (RW, 6);
      (RWX, 7);
      (RWL, 14);
      (RWLX, 15);
      (URW, 22);
      (URWX, 23);
      (URWL, 30);
      (URWLX, 31);
    ]

let locality_scalar =
  metadata_scalar ~name:"mCerise locality" [ (Directed, 0); (Local, 1); (Global, 2) ]

let permission_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"permission" ~tag:0
    ~wrong_tag_error:"not a mCerise permission encoding"
    ~malformed_payload_error:"unknown mCerise permission"
    (Tagged_metadata_codec.scalar_payload permission_scalar)

let locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"locality" ~tag:2
    ~wrong_tag_error:"not a mCerise locality encoding"
    ~malformed_payload_error:"unknown mCerise locality"
    (Tagged_metadata_codec.scalar_payload locality_scalar)

let permission_locality_pattern =
  Tagged_metadata_codec.encoding_pattern ~name:"permission/locality" ~tag:4
    ~wrong_tag_error:"not a mCerise permission/locality encoding"
    ~malformed_payload_error:"unknown mCerise permission/locality"
    (Tagged_metadata_codec.packed_pair ~low_width:5 ~high_width:2 permission_scalar locality_scalar)

let metadata_layout =
  match
    Tagged_metadata_codec.compile
      [
        Tagged_metadata_codec.pattern permission_pattern;
        Tagged_metadata_codec.pattern locality_pattern;
        Tagged_metadata_codec.pattern permission_locality_pattern;
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

let encode_locality (locality : locality) : Z.t = encode_metadata locality_pattern locality

let decode_locality (encoded : Z.t) : (locality, string) result =
  Tagged_metadata_codec.decode metadata_layout locality_pattern encoded

let encode_permission_locality (permission : permission) (locality : locality) : Z.t =
  encode_metadata permission_locality_pattern (permission, locality)

let decode_permission_locality (encoded : Z.t) : (permission * locality, string) result =
  Tagged_metadata_codec.decode metadata_layout permission_locality_pattern encoded
