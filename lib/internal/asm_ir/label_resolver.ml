(* Label-address resolution over assembler IR. *)
open Asm_ir

exception Unknown_label of string

type environment = (string, int) Hashtbl.t

(* Labels are normally sparse relative to operations. This is only an allocation hint; the table
   grows automatically if a program contains more labels. *)
let initial_label_capacity : int = 32

(* Record each label at the address of the next emitted operation and return the complete table. *)
let compute_environment (program : t) : environment =
  let environment = Hashtbl.create initial_label_capacity in
  let rec collect (address : int) (operations : machine_op list) : unit =
    match operations with
    | [] -> ()
    | Lbl name :: rest ->
        Hashtbl.replace environment name address;
        collect address rest
    | _operation :: rest -> collect (address + 1) rest
  in
  collect 0 program;
  environment

(* Return the numeric address assigned to a label or report an unknown reference. *)
let label_address (environment : environment) (name : string) : Z.t =
  match Hashtbl.find_opt environment name with
  | Some address -> Z.of_int address
  | None -> raise (Unknown_label name)

(* Replace labels recursively while preserving arithmetic expression constructors. *)
let rec resolve_expression (environment : environment) (expression : expr) : expr =
  match expression with
  | Label name -> IntLit (label_address environment name)
  | AddOp (left, right) ->
      AddOp (resolve_expression environment left, resolve_expression environment right)
  | SubOp (left, right) ->
      SubOp (resolve_expression environment left, resolve_expression environment right)
  | IntLit _ | CurrentAddr | Symbol _ | ExprParam _ -> expression

(* Remove a label declaration while preserving every emitted operation. *)
let remove_label_declaration (operation : machine_op) : machine_op option =
  match operation with Lbl _ -> None | _ -> Some operation

(* Return label-free IR with every label reference replaced by its integer address. *)
let resolve (program : t) : t =
  let environment = compute_environment program in
  program
  |> Ir_expression_mapper.map_program (resolve_expression environment)
  |> List.filter_map remove_label_declaration
