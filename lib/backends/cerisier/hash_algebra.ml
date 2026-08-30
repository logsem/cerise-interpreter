(** Deterministic compositional hashing for the Cerisier machine.

    The Rocq model leaves hashing abstract, but assumes that hashing a singleton agrees with hashing
    its element and that list hashing composes through an associative [hash_concat]. A hash value
    therefore records both a sequence length and a polynomial digest. This is an executable
    algebraic model, not a cryptographic hash. *)

open Ast

let modulus = Z.pred (Z.shift_left Z.one 127)
let base = Z.of_int 257
let modulo value = Z.erem value modulus

let zigzag value =
  if Z.sign value >= 0 then Z.mul (Z.of_int 2) value else Z.pred (Z.mul (Z.of_int 2) (Z.neg value))

let mix digest component = modulo Z.(add (mul digest base) (modulo component))
let digest fields = List.fold_left mix Z.zero fields
let tag value = Z.of_int value

let permission_tag = function
  | O -> tag 1
  | E -> tag 2
  | RO -> tag 3
  | RX -> tag 4
  | RW -> tag 5
  | RWX -> tag 6

let seal_permission_tag (can_seal, can_unseal) =
  tag (1 + (if can_seal then 2 else 0) + if can_unseal then 1 else 0)

let address_digest address = digest [ tag 1; zigzag address ]

let word_digest = function
  | I value -> digest [ tag 2; zigzag value ]
  | Sealable (Cap (permission, base, limit, cursor)) ->
      digest [ tag 3; permission_tag permission; zigzag base; zigzag limit; zigzag cursor ]
  | Sealable (SealRange (permission, base, limit, cursor)) ->
      digest [ tag 4; seal_permission_tag permission; zigzag base; zigzag limit; zigzag cursor ]
  | Sealed (object_type, Cap (permission, base, limit, cursor)) ->
      digest
        [
          tag 5;
          zigzag object_type;
          permission_tag permission;
          zigzag base;
          zigzag limit;
          zigzag cursor;
        ]
  | Sealed (object_type, SealRange (permission, base, limit, cursor)) ->
      digest
        [
          tag 6;
          zigzag object_type;
          seal_permission_tag permission;
          zigzag base;
          zigzag limit;
          zigzag cursor;
        ]

let encode length digest = Z.add (Z.mul length modulus) digest
let decode value = if Z.sign value < 0 then None else Some (Z.ediv_rem value modulus)

let pow_base exponent =
  let rec loop accumulator factor exponent =
    if Z.equal exponent Z.zero then accumulator
    else
      let accumulator =
        if Z.testbit exponent 0 then modulo (Z.mul accumulator factor) else accumulator
      in
      loop accumulator (modulo (Z.mul factor factor)) (Z.shift_right exponent 1)
  in
  loop Z.one base exponent

let concat left right =
  match (decode left, decode right) with
  | Some (left_length, left_digest), Some (right_length, right_digest) ->
      let combined_digest = modulo Z.(add (mul left_digest (pow_base right_length)) right_digest) in
      Some (encode (Z.add left_length right_length) combined_digest)
  | None, _ | _, None -> None

let singleton digest = encode Z.one digest
let address value = singleton (address_digest value)
let word value = singleton (word_digest value)

let words values =
  List.fold_left
    (fun accumulated value ->
      match concat accumulated (word value) with Some combined -> combined | None -> assert false)
    Z.zero values
