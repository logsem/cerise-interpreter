open Libmachine
type compiled_prog = (Ast.regname * Ast.word) list * (Z.t * Ast.word) list
type linkable_unit_type = CeriseObj of Ir_linkable_object.t | WasmModule of Wasm.Ir_wasm.ws_module

module ConvertInterface : sig
    val compile : Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t
      -> linkable_unit_type list -> compiled_prog
  end
