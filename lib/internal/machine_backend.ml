module type S = sig
  module MemMap = Machine.MemMap
  module RegMap = Machine.RegMap
  module SRegMap = Machine.SRegMap

  type exec_state = Running | Halted | Failed
  type reg_state = Ast.word RegMap.t
  type sreg_state = Ast.word SRegMap.t
  type mem_state = Ast.word MemMap.t
  type t

  val init_reg_state : reg_state
  val init_reg_state_zeros : reg_state
  val init_sreg_state_zeros : sreg_state
  val init_mem_state : Z.t -> Ast.t -> mem_state
  val init : reg_state -> sreg_state -> mem_state -> t
  val check_init_config : t -> unit
  val step : t -> t option
  val step_n : t -> int -> t option
  val run : t -> t
  val get_exec_state : t -> exec_state
  val get_regfile : t -> reg_state
  val get_sregfile : t -> sreg_state
  val get_memory : t -> mem_state
  val decode_machine_op : Z.t -> Ast.machine_op

  exception DecodeException of string
  exception CheckInitFailed of Ast.word
end

type choice = Default | Extracted

let select = function
  | Default -> (module Machine : S)
  | Extracted -> (module Machine_extracted : S)
