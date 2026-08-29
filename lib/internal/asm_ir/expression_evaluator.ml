(* Arithmetic-expression evaluation over assembler IR. *)
open Asm_ir

(* Evaluate an expression recursively. Label and symbol nodes indicate a skipped earlier pass. *)
let rec evaluate_expression (expression : expr) : Z.t =
  match expression with
  | IntLit value -> value
  | AddOp (left, right) -> Z.(evaluate_expression left + evaluate_expression right)
  | SubOp (left, right) -> Z.(evaluate_expression left - evaluate_expression right)
  | CurrentAddr | Label _ | Symbol _ | ExprParam _ ->
      raise (UnresolvedExpressionException expression)

(* Evaluate one complete expression and return its literal IR representation. *)
let evaluate_to_literal (expression : expr) : expr = IntLit (evaluate_expression expression)

(* Return IR in which every arithmetic expression has been reduced to an integer literal. *)
let evaluate (program : t) : t = Ir_expression_mapper.map_program evaluate_to_literal program
