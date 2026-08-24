;; Generated from logsem/griotte theories/switcher/switcher.v.
switcher:
    #[SU, Global, 9, 10, 9]
switcher_cc:
    getp ct2 csp
    mov ctp [R WL LG LM]
    sub ct2 ct2 ctp
    jnz ct2 switcher_force_unwind
    getl ct2 csp
    mov ctp Local
    sub ct2 ct2 ctp
    jnz ct2 switcher_force_unwind
    store csp cs0
    lea csp 1
    store csp cs1
    lea csp 1
    store csp cra
    lea csp 1
    store csp cgp
    lea csp 1
    readsr ct2 mtdc
    geta cs0 ct2
    add cs0 cs0 1
    gete ctp ct2
    lt ctp cs0 ctp
    jnz ctp 2
    jmp switcher_trusted_stack_exhausted
    lea ct2 1
    store ct2 csp
    writesr mtdc ct2
    gete cs0 csp
    geta cs1 csp
    subseg csp cs1 cs0
switcher_zero_stk_init_pre:
    sub cs0 cs1 cs0
    mov cs1 csp
switcher_zero_stk_loop_pre:
    jnz cs0 2
    jmp (switcher_zero_stk_end_pre - switcher_zero_stk_loop_pre - 1)
    store cs1 0
    lea cs1 1
    add cs0 cs0 1
switcher_zero_stk_loop_end_pre:
    jmp (switcher_zero_stk_loop_pre - switcher_zero_stk_loop_end_pre)
switcher_zero_stk_end_pre:
    getb cs1 PC
    geta cs0 PC
    sub cs1 cs1 cs0
    mov cs0 PC
    lea cs0 cs1
    lea cs0 -2
    load cs0 cs0
    unseal ct1 cs0 ct1
    load cs0 ct1
    land ct2 cs0 7
    lshiftr cs0 cs0 3
    getb cgp ct1
    geta cs1 ct1
    sub cs1 cgp cs1
    lea ct1 cs1
    load cra ct1
    lea ct1 1
    load cgp ct1
    lea cra cs0
    add ct2 ct2 1
    jmp ct2
    mov ca0 0
    mov ca1 0
    mov ca2 0
    mov ca3 0
    mov ca4 0
    mov ca5 0
    mov ct0 0
    mov cnull 0
    mov ctp 0
    mov ct1 0
    mov ct2 0
    mov cs0 0
    mov cs1 0
    mov ca6 0
    mov ca7 0
    mov cs2 0
    mov cs3 0
    mov cs4 0
    mov cs5 0
    mov cs6 0
    mov cs7 0
    mov cs8 0
    mov cs9 0
    mov cs10 0
    mov cs11 0
    mov ct3 0
    mov ct4 0
    mov ct5 0
    mov ct6 0
    jalr cra cra
switcher_after_compartment_call:
    readsr ctp mtdc
    load csp ctp
    lea ctp -1
    writesr mtdc ctp
    lea csp -1
    load cgp csp
    lea csp -1
    load cra csp
    lea csp -1
    load cs1 csp
    lea csp -1
    load cs0 csp
switcher_zero_stk_init_post:
    gete ct0 csp
    geta ct1 csp
    sub ct0 ct1 ct0
    mov ct1 csp
switcher_zero_stk_loop_post:
    jnz ct0 2
    jmp (switcher_zero_stk_end_post - switcher_zero_stk_loop_post - 1)
    store ct1 0
    lea ct1 1
    add ct0 ct0 1
switcher_zero_stk_loop_end_post:
    jmp (switcher_zero_stk_loop_post - switcher_zero_stk_loop_end_post)
switcher_zero_stk_end_post:
switcher_callee_dead_zeros:
    mov ct0 0
    mov cnull 0
    mov ctp 0
    mov ct1 0
    mov ct2 0
    mov ct3 0
    mov ct4 0
    mov ct5 0
    mov ct6 0
    mov ca2 0
    mov ca3 0
    mov ca4 0
    mov ca5 0
    mov ca6 0
    mov ca7 0
    mov cs2 0
    mov cs3 0
    mov cs4 0
    mov cs5 0
    mov cs6 0
    mov cs7 0
    mov cs8 0
    mov cs9 0
    mov cs10 0
    mov cs11 0
    jalr cnull cra
switcher_trusted_stack_exhausted:
    lea csp -1
    load cgp csp
    lea csp -1
    load cra csp
    lea csp -1
    load cs1 csp
    lea csp -1
    load cs0 csp
    mov ca0 -141
    mov ca1 0
    jmp switcher_callee_dead_zeros
switcher_force_unwind:
    mov ca0 -1
    mov ca1 0
    jmp switcher_after_compartment_call
switcher_end:
