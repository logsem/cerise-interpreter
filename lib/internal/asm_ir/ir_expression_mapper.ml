(* Shared expression traversal for assembler IR passes. *)
open Asm_ir

type expression_mapper = expr -> expr

(* Apply an expression mapper to an encoded constant when it contains an expression. *)
let map_constant (map_expression : expression_mapper) (constant : const_encoded) : const_encoded =
  match constant with
  | ConstExpr expression -> ConstExpr (map_expression expression)
  | _ -> constant

(* Apply an expression mapper to every expression reachable through an instruction value. *)
let map_value (map_expression : expression_mapper) (value : reg_or_const) : reg_or_const =
  match value with
  | Const constant -> Const (map_constant map_expression constant)
  | Register _ | ValueParam _ -> value

(* Apply an expression mapper to every expression field of a capability or sealing range. *)
let map_sealable (map_expression : expression_mapper) (sealable : sealable) : sealable =
  match sealable with
  | Cap (permission, locality, base, ending, address) ->
      Cap (permission, locality, map_expression base, map_expression ending, map_expression address)
  | SealRange (permission, locality, base, ending, address) ->
      SealRange
        (permission, locality, map_expression base, map_expression ending, map_expression address)

(* Apply an expression mapper to every expression field of a literal word. *)
let map_word (map_expression : expression_mapper) (word : word) : word =
  match word with
  | I expression -> I (map_expression expression)
  | Sealable sealable -> Sealable (map_sealable map_expression sealable)
  | Sentry (permission, locality, base, ending, address) ->
      Sentry
        (permission, locality, map_expression base, map_expression ending, map_expression address)
  | Sealed (otype, sealable) -> Sealed (map_expression otype, map_sealable map_expression sealable)

(* Apply an expression mapper to every expression-bearing operand of one ordinary operation. *)
let map_operation (map_expression : expression_mapper) (operation : machine_op) : machine_op =
  match operation with
  | Jmp value -> Jmp (map_value map_expression value)
  | Jnz (register, value) -> Jnz (register, map_value map_expression value)
  | Move (register, value) -> Move (register, map_value map_expression value)
  | Store (register, value) -> Store (register, map_value map_expression value)
  | Add (register, left, right) ->
      Add (register, map_value map_expression left, map_value map_expression right)
  | Sub (register, left, right) ->
      Sub (register, map_value map_expression left, map_value map_expression right)
  | Mul (register, left, right) ->
      Mul (register, map_value map_expression left, map_value map_expression right)
  | Rem (register, left, right) ->
      Rem (register, map_value map_expression left, map_value map_expression right)
  | Div (register, left, right) ->
      Div (register, map_value map_expression left, map_value map_expression right)
  | LAnd (register, left, right) ->
      LAnd (register, map_value map_expression left, map_value map_expression right)
  | LOr (register, left, right) ->
      LOr (register, map_value map_expression left, map_value map_expression right)
  | LShiftL (register, left, right) ->
      LShiftL (register, map_value map_expression left, map_value map_expression right)
  | LShiftR (register, left, right) ->
      LShiftR (register, map_value map_expression left, map_value map_expression right)
  | Lt (register, left, right) ->
      Lt (register, map_value map_expression left, map_value map_expression right)
  | Lea (register, value) -> Lea (register, map_value map_expression value)
  | Restrict (register, value) -> Restrict (register, map_value map_expression value)
  | SubSeg (register, base, ending) ->
      SubSeg (register, map_value map_expression base, map_value map_expression ending)
  | Word word -> Word (map_word map_expression word)
  | Define _ | MacroDef _ | MacroCall _ ->
      raise (UnexpandedMacroException "macro construct during expression traversal")
  | Lbl _ | Jalr _ | ReadSR _ | WriteSR _ | Load _ | GetL _ | GetB _ | GetE _ | GetA _ | GetP _
  | GetOType _ | GetWType _ | Seal _ | UnSeal _ | Fail | Halt ->
      operation

(* Return a program with the mapper applied to every expression position. *)
let map_program (map_expression : expression_mapper) (program : t) : t =
  List.map (map_operation map_expression) program
