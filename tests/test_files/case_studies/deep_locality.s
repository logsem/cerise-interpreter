;; Deep locality: a nested capability loaded through RW-DL becomes local and
;; cannot be retained in the adversary's global state.
loader:
    #([X Ow LG LM], Global, A, A_end, A_main)
    #([R W LG LM], Global, A_data, A_data_end, A_data)
loader_main:
    mov cra PC
    lea cra -1
    load cgp cra
    lea cra -1
    load cra cra
    jalr cra cra
    fail
loader_end:

A:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
    #(E-[X Ow LG LM], Global, assert, assert_end, assert)
    #{9: ([R Ow LG LM], Global, B_ext, B_ext_end, B_ext_f)}
A_main:
    store cgp 0
    mov ct0 cgp
    getb ct1 cgp
    add ct2 ct1 1
    subseg ct0 ct1 ct2
    lea cgp 1
    store cgp ct0
    mov ca0 cgp
    lea cgp -1
    add ct1 ct2 1
    subseg ca0 ct2 ct1
    restrict ca0 ([R W DL LM], Local)
    ;; fetch switcher and B.f
    mov ct0 PC
    getb ct1 ct0
    geta ct2 ct0
    sub ct1 ct1 ct2
    lea ct0 ct1
    load ct0 ct0
    mov ct1 PC
    getb ct2 ct1
    geta ct3 ct1
    sub ct2 ct2 ct3
    lea ct1 ct2
    lea ct1 2
    load ct1 ct1
    mov cs0 ct0
    mov cs1 ct1
    jalr cra ct0
    store cgp 42
    mov ca0 0
    mov ct0 cs0
    mov ct1 cs1
    jalr cra ct0
    load ct0 cgp
    mov ct1 42
    ;; assert(ct0 == ct1)
    mov ct2 PC
    getb ct3 ct2
    geta ct4 ct2
    sub ct3 ct3 ct4
    lea ct2 ct3
    lea ct2 1
    load ct2 ct2
    mov ct3 cra
    jalr cra ct2
    mov cra ct3
    mov ct3 0
    mov ct2 0
    halt
A_end:

B:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
B_f:
    getwtype ct0 ca0
    sub ct0 ct0 Cap
    jnz ct0 (B_f_return - &CURRENT_ADDR)
    ;; This write is possible during the first call. The capability cannot be
    ;; saved globally for use during the second call because it is deep-local.
    load ct0 ca0
    store ct0 7
B_f_return:
    jalr cnull cra
B_end:

A_data:
    #0
    #0
A_data_end:
B_data:
    #0
B_data_end:
A_ext:
    #([X Ow LG LM], Global, A, A_end, A)
    #([R W LG LM], Global, A_data, A_data_end, A_data)
A_ext_end:
B_ext:
    #([X Ow LG LM], Global, B, B_end, B)
    #([R W LG LM], Global, B_data, B_data_end, B_data)
B_ext_f: #(((B_f - B) << 3) || 1)
B_ext_end:

assert:
    sub ct0 ct0 ct1
    jnz ct0 (assert_fail - &CURRENT_ADDR)
assert_success:
    mov ct0 0
    mov ct1 0
    jalr cnull cra
assert_fail:
    mov ct1 PC
    ;; ct1 contains the address of the preceding mov, hence the +1.
    lea ct1 (assert_flag_cap + 1 - &CURRENT_ADDR)
    load ct1 ct1
    store ct1 1
    mov ct0 0
    mov ct1 0
    jalr cnull cra
assert_flag_cap:
    #([R W LG LM], Global, assert_flag, assert_data_end, assert_flag)
assert_end:
assert_flag:
    #0
assert_data_end:

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
