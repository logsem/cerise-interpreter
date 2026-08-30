open Ast

module RegMap = Map.Make (struct
  type t = register

  let compare (left : t) (right : t) : int = Stdlib.compare left right
end)

module SRegMap = Map.Make (struct
  type t = system_register

  let compare (left : t) (right : t) : int = Stdlib.compare left right
end)

module MemMap = Map.Make (Z)

type status = Running | Halted | Failed

type t = {
  config : Runtime_config.t;
  status : status;
  registers : word RegMap.t;
  system_registers : word SRegMap.t;
  memory : word MemMap.t;
}

let diagnostic (message : string) : ('a, Diagnostic.t list) result = Error [ Diagnostic.error message ]
let ( let* ) (type value next error) (result : (value, error) result)
        (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation
let arch_root_memory_permission = (R, WL, LG, LM)
let arch_root_executable_permission = (XSR, Ow, LG, LM)

let rx_flows (requested : rx_permission) (current : rx_permission) : bool =
  match (requested, current) with
  | Orx, _ -> true
  | R, Orx -> false
  | R, _ -> true
  | X, (X | XSR) -> true
  | X, _ -> false
  | XSR, XSR -> true
  | XSR, _ -> false

let write_flows (requested : write_permission) (current : write_permission) : bool =
  match (requested, current) with
  | Ow, _ -> true
  | W, Ow -> false
  | W, _ -> true
  | WL, WL -> true
  | WL, _ -> false

let deep_local_flows (requested : deep_local_permission) (current : deep_local_permission) : bool =
  match (requested, current) with DL, _ -> true | LG, LG -> true | LG, DL -> false

let deep_read_only_flows (requested : deep_read_only_permission) (current : deep_read_only_permission) : bool =
  match (requested, current) with DRO, _ -> true | LM, LM -> true | LM, DRO -> false

let permission_flows ((rx, write, deep_local, deep_read_only) : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission)
    ((rx', write', deep_local', deep_read_only') : rx_permission * write_permission * deep_local_permission *
deep_read_only_permission) : bool =
  rx_flows rx rx' && write_flows write write'
  && deep_local_flows deep_local deep_local'
  && deep_read_only_flows deep_read_only deep_read_only'

let permission_of_word (matched_value : word) : permission = match matched_value with
  | Sealable (Cap (permission, _, _, _, _))
  | Sentry (permission, _, _, _, _)
  | Sealed (_, Cap (permission, _, _, _, _)) ->
      permission
  | _ -> null_permission

let word_is_derived (word : word) : bool =
  permission_flows (permission_of_word word) arch_root_executable_permission
  || permission_flows (permission_of_word word) arch_root_memory_permission

let zero_registers (() : unit) : word RegMap.t =
  let registers =
    List.init 32 (fun number -> (Reg number, I Z.zero)) |> List.to_seq |> RegMap.of_seq
  in
  registers |> RegMap.add PC (I Z.zero) |> RegMap.add cnull (I Z.zero)

let initial_registers (config : Runtime_config.t) : word RegMap.t =
  let limit = Runtime_config.max_addr config in
  zero_registers ()
  |> RegMap.add PC (Sealable (Cap (arch_root_executable_permission, Global, Z.zero, limit, Z.zero)))
  |> RegMap.add cgp (Sealable (Cap (arch_root_memory_permission, Global, Z.zero, limit, Z.zero)))
  |> RegMap.add ca3 (Sealable (SealRange ((true, true), Global, Z.zero, max_object_type, Z.zero)))
  |> RegMap.add cnull (I Z.zero)

let read_register (register : register) (state : t) : word =
  match register with Reg 0 -> I Z.zero | _ -> RegMap.find register state.registers

let set_register (register : register) (word : word) (state : t) : t =
  match register with
  | Reg 0 -> { state with registers = RegMap.add cnull (I Z.zero) state.registers }
  | _ -> { state with registers = RegMap.add register word state.registers }

let set_system_register (register : system_register) (word : word) (state : t) : t =
  { state with system_registers = SRegMap.add register word state.system_registers }

let set_memory_raw (address : Z.t) (word : word) (state : t) : t = { state with memory = MemMap.add address word state.memory }

let init (config : Runtime_config.t) (program : word list) (regfile : ((register * word) list * (system_register * word) list) option) : (t, Diagnostic.t list) result =
  let registers =
    match regfile with None -> initial_registers config | Some _ -> zero_registers ()
  in
  let system_registers = SRegMap.singleton MTDC (I Z.zero) in
  let register_words, system_register_words = Option.value regfile ~default:([], []) in
  let* registers =
    List.fold_left
      (fun result (register, word) ->
        let* registers = result in
        if word_is_derived word then Ok (RegMap.add register word registers)
        else
          diagnostic
            (Printf.sprintf "Initial value for %s is not derived from a Griotte architectural root."
               (Printer.register register)))
      (Ok registers) register_words
  in
  let registers = RegMap.add cnull (I Z.zero) registers in
  let* system_registers =
    List.fold_left
      (fun result (register, word) ->
        let* system_registers = result in
        if word_is_derived word then Ok (SRegMap.add register word system_registers)
        else diagnostic "Initial MTDC value is not derived from a Griotte architectural root.")
      (Ok system_registers) system_register_words
  in
  let* memory =
    List.fold_left
      (fun result word ->
        let* address, memory = result in
        if word_is_derived word then Ok (Z.succ address, MemMap.add address word memory)
        else
          diagnostic
            (Printf.sprintf
               "Initial program word at address %s is not derived from a Griotte architectural \
                root."
               (Z.to_string address)))
      (Ok (Z.zero, MemMap.empty))
      program
    |> Result.map snd
  in
  Ok { config; status = Running; registers; system_registers; memory }
