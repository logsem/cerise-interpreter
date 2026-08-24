;; Counter: increment private state, call an untrusted compartment, then return.
loader:
    #([X Ow LG LM], Global, Counter, Counter_end, Counter_main)
    #([R W LG LM], Global, Counter_data, Counter_data_end, Counter_data)
loader_main:
    mov cra PC
    lea cra -1
    load cgp cra
    lea cra -1
    load cra cra
    jalr cra cra
    halt
loader_end:

Counter:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
    #{9: ([R Ow LG LM], Global, C_ext, C_ext_end, C_ext_f)}
Counter_main:
    load cs0 cgp
    add cs0 cs0 1
    store cgp cs0
    ;; fetch switcher and C.f
    mov ct0 PC
    getb cs0 ct0
    geta cs1 ct0
    sub cs0 cs0 cs1
    lea ct0 cs0
    load ct0 ct0
    mov cs0 0
    mov cs1 0
    mov ct1 PC
    getb cs0 ct1
    geta cs1 ct1
    sub cs0 cs0 cs1
    lea ct1 cs0
    lea ct1 1
    load ct1 ct1
    mov cs0 cra
    jalr cra ct0
    mov cra cs0
    mov ca0 0
    mov ca1 0
    mov cs0 0
    mov cs1 0
    jalr cnull cra
Counter_end:

C:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
C_f:
    ;; The callback can touch only its own data.
    store cgp 42
    jalr cnull cra
C_end:

Counter_data:
    #0
Counter_data_end:
C_data:
    #0
C_data_end:

Counter_ext:
    #([X Ow LG LM], Global, Counter, Counter_end, Counter)
    #([R W LG LM], Global, Counter_data, Counter_data_end, Counter_data)
Counter_ext_end:
C_ext:
    #([X Ow LG LM], Global, C, C_end, C)
    #([R W LG LM], Global, C_data, C_data_end, C_data)
C_ext_f: #(((C_f - C) << 3) || 0)
C_ext_end:

;; Griotte trusted switcher.
switcher:
    #[SU, Global, 9, 10, 9]
switcher_cc:
    getp ct2 csp
    mov ctp [R WL LG LM]
    sub ct2 ct2 ctp
    jnz ct2 (switcher_force_unwind - &CURRENT_ADDR)
    getl ct2 csp
    mov ctp Local
    sub ct2 ct2 ctp
    jnz ct2 (switcher_force_unwind - &CURRENT_ADDR)
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
    jmp (switcher_trusted_stack_exhausted - &CURRENT_ADDR)
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
    mov r10 0
    mov r11 0
    mov r12 0
    mov r13 0
    mov r14 0
    mov r15 0
    mov r5 0
    mov r0 0
    mov r4 0
    mov r6 0
    mov r7 0
    mov r8 0
    mov r9 0
    mov r16 0
    mov r17 0
    mov r18 0
    mov r19 0
    mov r20 0
    mov r21 0
    mov r22 0
    mov r23 0
    mov r24 0
    mov r25 0
    mov r26 0
    mov r27 0
    mov r28 0
    mov r29 0
    mov r30 0
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
    mov r0 0
    mov r4 0
    mov r5 0
    mov r6 0
    mov r7 0
    mov r12 0
    mov r13 0
    mov r14 0
    mov r15 0
    mov r16 0
    mov r17 0
    mov r18 0
    mov r19 0
    mov r20 0
    mov r21 0
    mov r22 0
    mov r23 0
    mov r24 0
    mov r25 0
    mov r26 0
    mov r27 0
    mov r28 0
    mov r29 0
    mov r30 0
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
    jmp (switcher_callee_dead_zeros - &CURRENT_ADDR)
switcher_force_unwind:
    mov ca0 -1
    mov ca1 0
    jmp (switcher_after_compartment_call - &CURRENT_ADDR)
switcher_end:
