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

let operand = Instruction_codec.register_or_constant register_codec Instruction_codec.zarith
let from_operand (matched_value : (register, Z.t) Instruction_codec.register_or_constant) : reg_or_const = match matched_value with Instruction_codec.Register r -> Register r | Constant z -> Constant z
let to_operand (matched_value : reg_or_const) : (register, Z.t) Instruction_codec.register_or_constant = match matched_value with Register r -> Instruction_codec.Register r | Constant z -> Constant z
let r = Instruction_codec.register register_codec
let rr = Instruction_codec.pair r r
let ro = Instruction_codec.pair r operand
let roo = Instruction_codec.triple r operand operand
let rro = Instruction_codec.triple r r operand

let case (name : string) (codec : 'a Instruction_codec.shape) (construct : ('a -> 'b)) (project : ('b -> 'a option)) : 'b Instruction_codec.case =
  Instruction_codec.case ~name codec ~construct ~project

let unit_case (name : string) (construct : 'a) (project : ('a -> bool)) : 'a Instruction_codec.case =
  case name Instruction_codec.unit (fun () -> construct)
    (fun x -> if project x then Some () else None)

let cases =
  [
    case "Jmp" r (fun x -> Jmp x) (function Jmp x -> Some x | _ -> None);
    case "Jnz" rr (fun (a,b) -> Jnz (a,b)) (function Jnz (a,b) -> Some (a,b) | _ -> None);
    case "Move" ro (fun (a,b) -> Move (a,from_operand b))
      (function Move (a,b) -> Some (a,to_operand b) | _ -> None);
    case "Load" rr (fun (a,b) -> Load (a,b)) (function Load (a,b) -> Some (a,b) | _ -> None);
    case "Store" ro (fun (a,b) -> Store (a,from_operand b))
      (function Store (a,b) -> Some (a,to_operand b) | _ -> None);
    case "Add" roo (fun (a,b,c) -> Add (a,from_operand b,from_operand c))
      (function Add (a,b,c) -> Some (a,to_operand b,to_operand c) | _ -> None);
    case "Sub" roo (fun (a,b,c) -> Sub (a,from_operand b,from_operand c))
      (function Sub (a,b,c) -> Some (a,to_operand b,to_operand c) | _ -> None);
    case "Lt" roo (fun (a,b,c) -> Lt (a,from_operand b,from_operand c))
      (function Lt (a,b,c) -> Some (a,to_operand b,to_operand c) | _ -> None);
    case "Lea" ro (fun (a,b) -> Lea (a,from_operand b))
      (function Lea (a,b) -> Some (a,to_operand b) | _ -> None);
    case "Restrict" ro (fun (a,b) -> Restrict (a,from_operand b))
      (function Restrict (a,b) -> Some (a,to_operand b) | _ -> None);
    case "SubSeg" roo (fun (a,b,c) -> SubSeg (a,from_operand b,from_operand c))
      (function SubSeg (a,b,c) -> Some (a,to_operand b,to_operand c) | _ -> None);
    case "IsPtr" rr (fun (a,b) -> IsPtr (a,b))
      (function IsPtr (a,b) -> Some (a,b) | _ -> None);
    case "GetP" rr (fun (a,b) -> GetP (a,b))
      (function GetP (a,b) -> Some (a,b) | _ -> None);
    case "GetL" rr (fun (a,b) -> GetL (a,b))
      (function GetL (a,b) -> Some (a,b) | _ -> None);
    case "GetB" rr (fun (a,b) -> GetB (a,b))
      (function GetB (a,b) -> Some (a,b) | _ -> None);
    case "GetE" rr (fun (a,b) -> GetE (a,b))
      (function GetE (a,b) -> Some (a,b) | _ -> None);
    case "GetA" rr (fun (a,b) -> GetA (a,b))
      (function GetA (a,b) -> Some (a,b) | _ -> None);
    unit_case "Fail" Fail (function Fail -> true | _ -> false);
    unit_case "Halt" Halt (function Halt -> true | _ -> false);
    case "LoadU" rro (fun (a,b,c) -> LoadU (a,b,from_operand c))
      (function LoadU (a,b,c) -> Some (a,b,to_operand c) | _ -> None);
    case "StoreU" roo (fun (a,b,c) -> StoreU (a,from_operand b,from_operand c))
      (function StoreU (a,b,c) -> Some (a,to_operand b,to_operand c) | _ -> None);
    case "PromoteU" r (fun a -> PromoteU a)
      (function PromoteU a -> Some a | _ -> None);
  ]

let table =
  match Instruction_codec.compile cases with
  | Ok table -> table
  | Error errors ->
      failwith (String.concat "; " (List.map Instruction_codec.error_message errors))

let encode = Instruction_codec.encode table
let decode = Instruction_codec.decode table
let allocations = Instruction_codec.allocations table

let encode_tag (tag : int) (payload : Z.t) : Z.t = Z.logor (Z.of_int tag) (Z.shift_left payload 3)

let permission_scalar (matched_value : permission) : int = match matched_value with
  | O -> 0 | E -> 1 | RO -> 4 | RX -> 5 | RW -> 6 | RWX -> 7
  | RWL -> 14 | RWLX -> 15 | URW -> 22 | URWX -> 23 | URWL -> 30 | URWLX -> 31

let encode_permission (p : permission) : Z.t = encode_tag 0 (Z.of_int (permission_scalar p))

let permission_of_scalar (matched_value : int) : permission option = match matched_value with
  | 0 -> Some O | 1 -> Some E | 4 -> Some RO | 5 -> Some RX | 6 -> Some RW
  | 7 -> Some RWX | 14 -> Some RWL | 15 -> Some RWLX | 22 -> Some URW
  | 23 -> Some URWX | 30 -> Some URWL | 31 -> Some URWLX | _ -> None

let decode_permission (z : Z.t) : (permission, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) Z.zero) then
    Error "not a mCerise permission encoding"
  else if not (Z.fits_int (Z.shift_right z 3)) then Error "unknown mCerise permission"
  else
    match permission_of_scalar (Z.to_int (Z.shift_right z 3)) with
    | Some p -> Ok p | None -> Error "unknown mCerise permission"

let locality_scalar (matched_value : locality) : int = match matched_value with Global -> 2 | Local -> 1 | Directed -> 0
let encode_locality (l : locality) : Z.t = encode_tag 2 (Z.of_int (locality_scalar l))

let decode_locality (z : Z.t) : (locality, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 2)) then
    Error "not a mCerise locality encoding"
  else
    match Z.shift_right z 3 with
    | p when Z.equal p Z.zero -> Ok Directed
    | p when Z.equal p Z.one -> Ok Local
    | p when Z.equal p (Z.of_int 2) -> Ok Global
    | _ -> Error "unknown mCerise locality"

let encode_permission_locality (p : permission) (l : locality) : Z.t =
  encode_tag 4 (Z.of_int ((locality_scalar l lsl 5) + permission_scalar p))

let decode_permission_locality (z : Z.t) : (permission * locality, string) result =
  if Z.sign z < 0 || not (Z.equal (Z.extract z 0 3) (Z.of_int 4)) then
    Error "not a mCerise permission/locality encoding"
  else
    let payload = Z.shift_right z 3 in
    if not (Z.fits_int payload) then Error "unknown mCerise permission/locality"
    else
      let payload = Z.to_int payload in
      match permission_of_scalar (payload land 31), payload lsr 5 with
      | Some p, 0 -> Ok (p, Directed)
      | Some p, 1 -> Ok (p, Local)
      | Some p, 2 -> Ok (p, Global)
      | _ -> Error "unknown mCerise permission/locality"
