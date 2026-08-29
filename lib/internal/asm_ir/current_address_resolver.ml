(* Resolution of [&CURRENT_ADDR] expressions over expanded assembler IR. *)
open Asm_ir

(* Replace [&CURRENT_ADDR] recursively with the address of its containing emitted operation. *)
let rec resolve_expression (address : int) (expression : expr) : expr =
  match expression with
  | CurrentAddr -> IntLit (Z.of_int address)
  | AddOp (left, right) -> AddOp (resolve_expression address left, resolve_expression address right)
  | SubOp (left, right) -> SubOp (resolve_expression address left, resolve_expression address right)
  | IntLit _ | Symbol _ | Label _ | ExprParam _ -> expression

(* Replace current-address expressions throughout the program. Labels do not occupy an address;
   every instruction and literal word does. Macro constructs must have been removed by expansion. *)
let resolve (program : t) : t =
  let rec resolve_operations (address : int) (operations : machine_op list) : machine_op list =
    match operations with
    | [] -> []
    | (Lbl _ as label) :: rest -> label :: resolve_operations address rest
    | operation :: rest ->
        let resolved = Ir_expression_mapper.map_operation (resolve_expression address) operation in
        resolved :: resolve_operations (address + 1) rest
  in
  resolve_operations 0 program
