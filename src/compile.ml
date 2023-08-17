open Libinterp

let full_compile filename_reg filename_asm prog =
  let (regs, mem) = Compiler.machine_compile prog in
  Compiler.output_machine filename_reg filename_asm regs mem

let () =
  full_compile
    "asm-toys/stack_loaded.reg"
    "asm-toys/stack_loaded.s"
    Extract.loaded_stack_example;
  full_compile
    "asm-toys/bank_loaded.reg"
    "asm-toys/bank_loaded.s"
    Extract.loaded_bank_example;
  full_compile
    "asm-toys/bank_unsafe_loaded.reg"
    "asm-toys/bank_unsafe_loaded.s"
    Extract.loaded_bank_unsafe_example;
  full_compile
    "asm-toys/dummy_loaded.reg"
    "asm-toys/dummy_loaded.s"
    Extract.loaded_dummy_example;
  full_compile
    "asm-toys/reg_alloc_loaded.reg"
    "asm-toys/reg_alloc_loaded.s"
    Extract.loaded_reg_alloc_example;
  full_compile
    "asm-toys/incr_loaded.reg"
    "asm-toys/incr_loaded.s"
    Extract.loaded_incr_example
