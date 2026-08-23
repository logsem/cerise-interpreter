open Ast
module MemMap : Map.S with type key = Z.t
module RegMap : Map.S with type key = regname

type exec_state = Running | Halted | Failed
type reg_state = word RegMap.t

(* Sparse backing map. Missing addressable words are read as integer zero by [read_mem]. *)
type mem_state = word MemMap.t
type exec_conf = { reg : reg_state; mem : mem_state }
type t

val init_reg_state : Z.t -> reg_state
val init_mem_state : Z.t -> Ast.t -> mem_state
val init : word RegMap.t -> word MemMap.t -> t
val step : t -> t option
val step_n : t -> int -> t option
val run : t -> t
val read_reg : regname -> t -> word
val read_mem : Z.t -> t -> word option
val set_reg : regname -> word -> t -> t
val set_mem : Z.t -> word -> t -> t
val get_exec_state : t -> exec_state
val get_exec_conf : t -> exec_conf
val get_regfile : t -> reg_state
val get_memory : t -> mem_state
val decode_machine_op : Z.t -> Ast.machine_op

exception DecodeException of string
