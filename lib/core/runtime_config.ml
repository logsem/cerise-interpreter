type t = { max_addr : Z.t; stack_addr : Z.t }

let legacy_max_addr = Z.div (Z.of_int32 Int32.max_int) (Z.of_int 4096)

let create ?(max_addr : Z.t = legacy_max_addr) ?stack_addr:(stack_addr : Z.t option)
    (() : unit) : t =
  if Z.sign max_addr <= 0 then invalid_arg "Runtime_config.create: max_addr must be positive";
  let stack_addr = Option.value stack_addr ~default:Z.(max_addr / of_int 2) in
  if Z.sign stack_addr < 0 || Z.compare stack_addr max_addr > 0 then
    invalid_arg "Runtime_config.create: stack_addr must lie between zero and max_addr";
  { max_addr; stack_addr }

let default = create ()
let max_addr (config : t) : Z.t = config.max_addr
let stack_addr (config : t) : Z.t = config.stack_addr
