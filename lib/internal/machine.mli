open Ast
module MemMap : Map.S with type key = Z.t
module RegMap : Map.S with type key = regname
module SRegMap : Map.S with type key = sregname

type exec_state = Running | Halted | Failed
type reg_state = word RegMap.t
type sreg_state = word SRegMap.t

(* Sparse backing map. Missing addressable words are read as integer zero by [read_mem]. *)
type mem_state = word MemMap.t
type exec_conf = { reg : reg_state; sreg : sreg_state; mem : mem_state }
type t

val init_reg_state : reg_state
val init_reg_state_zeros : reg_state
val init_sreg_state_zeros : sreg_state
val init_mem_state : Z.t -> Ast.t -> mem_state
val init : word RegMap.t -> word SRegMap.t -> word MemMap.t -> t
val check_init_config : t -> unit
val step : t -> t option
val step_n : t -> int -> t option
val run : t -> t
val read_reg : regname -> t -> word
val read_sreg : sregname -> t -> word
val read_mem : Z.t -> t -> word option
val set_reg : regname -> word -> t -> t
val set_mem : Z.t -> word -> t -> t
val get_exec_state : t -> exec_state
val get_exec_conf : t -> exec_conf
val get_regfile : t -> reg_state
val get_sregfile : t -> sreg_state
val get_memory : t -> mem_state
val decode_machine_op : Z.t -> Ast.machine_op

exception DecodeException of string
exception CheckInitFailed of Ast.word
