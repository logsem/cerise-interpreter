module type S = sig
  module MemMap = Machine.MemMap
  module RegMap = Machine.RegMap

  type exec_state = Running | Halted | Failed
  type reg_state = Ast.word RegMap.t
  type mem_state = Ast.word MemMap.t
  type exec_conf = { reg : reg_state; mem : mem_state }
  type t

  val init_reg_state : Z.t -> reg_state
  val init_mem_state : Z.t -> Ast.t -> mem_state
  val init : reg_state -> mem_state -> t
  val step : t -> t option
  val step_n : t -> int -> t option
  val run : t -> t
  val read_reg : Ast.regname -> t -> Ast.word
  val read_mem : Z.t -> t -> Ast.word option
  val set_reg : Ast.regname -> Ast.word -> t -> t
  val set_mem : Z.t -> Ast.word -> t -> t
  val get_exec_state : t -> exec_state
  val get_exec_conf : t -> exec_conf
  val get_regfile : t -> reg_state
  val get_memory : t -> mem_state
  val decode_machine_op : Z.t -> Ast.machine_op

  exception DecodeException of string
end

module Default = Machine

type choice = Default

let select = function Default -> (module Default : S)
