open Libmachine
type compiled_prog = (Ast.regname * Ast.word) list * (Z.t * Ast.word) list

module ConvertInterface : sig
    val compile : Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t
      -> Wasm.Ir_wasm.ws_module list -> Ir_linkable_object.t list
      -> compiled_prog
  end
