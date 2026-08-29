open Ast

module RegMap = Map.Make (struct
  type t = register

  let compare = compare
end)

module SRegMap = Map.Make (struct
  type t = system_register

  let compare = compare
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

let diagnostic message = Error [ Diagnostic.error message ]
let ( let* ) = Result.bind
let arch_root_memory_permission = (R, WL, LG, LM)
let arch_root_executable_permission = (XSR, Ow, LG, LM)

let rx_flows requested current =
  match (requested, current) with
  | Orx, _ -> true
  | R, Orx -> false
  | R, _ -> true
  | X, (X | XSR) -> true
  | X, _ -> false
  | XSR, XSR -> true
  | XSR, _ -> false

let write_flows requested current =
  match (requested, current) with
  | Ow, _ -> true
  | W, Ow -> false
  | W, _ -> true
  | WL, WL -> true
  | WL, _ -> false

let deep_local_flows requested current =
  match (requested, current) with DL, _ -> true | LG, LG -> true | LG, DL -> false

let deep_read_only_flows requested current =
  match (requested, current) with DRO, _ -> true | LM, LM -> true | LM, DRO -> false

let permission_flows (rx, write, deep_local, deep_read_only)
    (rx', write', deep_local', deep_read_only') =
  rx_flows rx rx' && write_flows write write'
  && deep_local_flows deep_local deep_local'
  && deep_read_only_flows deep_read_only deep_read_only'

let permission_of_word = function
  | Sealable (Cap (permission, _, _, _, _))
  | Sentry (permission, _, _, _, _)
  | Sealed (_, Cap (permission, _, _, _, _)) ->
      permission
  | _ -> null_permission

let word_is_derived word =
  permission_flows (permission_of_word word) arch_root_executable_permission
  || permission_flows (permission_of_word word) arch_root_memory_permission

let zero_registers () =
  let registers =
    List.init 32 (fun number -> (Reg number, I Z.zero)) |> List.to_seq |> RegMap.of_seq
  in
  registers |> RegMap.add PC (I Z.zero) |> RegMap.add cnull (I Z.zero)

let initial_registers config =
  let limit = Runtime_config.max_addr config in
  zero_registers ()
  |> RegMap.add PC (Sealable (Cap (arch_root_executable_permission, Global, Z.zero, limit, Z.zero)))
  |> RegMap.add cgp (Sealable (Cap (arch_root_memory_permission, Global, Z.zero, limit, Z.zero)))
  |> RegMap.add ca3 (Sealable (SealRange ((true, true), Global, Z.zero, max_object_type, Z.zero)))
  |> RegMap.add cnull (I Z.zero)

let read_register register state =
  match register with Reg 0 -> I Z.zero | _ -> RegMap.find register state.registers

let set_register register word state =
  match register with
  | Reg 0 -> { state with registers = RegMap.add cnull (I Z.zero) state.registers }
  | _ -> { state with registers = RegMap.add register word state.registers }

let set_system_register register word state =
  { state with system_registers = SRegMap.add register word state.system_registers }

let set_memory_raw address word state = { state with memory = MemMap.add address word state.memory }

let init config program regfile =
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
