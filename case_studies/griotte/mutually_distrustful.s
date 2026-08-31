%macro fetch(offset: expr, dst: reg, scratch1: reg, scratch2: reg)
    mov $dst PC
    getb $scratch1 $dst
    geta $scratch2 $dst
    sub $scratch1 $scratch1 $scratch2
    lea $dst $scratch1
    lea $dst $offset
    load $dst $dst
    mov $scratch1 0
    mov $scratch2 0
%endmacro

%macro assert_eq(offset: expr, dst: reg, scratch1: reg, scratch2: reg)
    mov $dst PC
    getb $scratch1 $dst
    geta $scratch2 $dst
    sub $scratch1 $scratch1 $scratch2
    lea $dst $scratch1
    lea $dst $offset
    load $dst $dst
    mov $scratch1 0
    mov $scratch2 0
    mov $scratch1 cra
    jalr cra $dst
    mov cra $scratch1
    mov $scratch1 0
    mov $dst 0
%endmacro

    ;; TODO Something that could be cool !!
    ;; is to randomly generate code for B_f and C_g,
    ;; and run the tests for all generated code,
    ;; and have an assert flag in A,
    ;; and verify that for any executions, the flag does not change


    ;; main is just a very basic loader that jumps to the main compartment A
    ;; and terminates the machine
loader:
    #([X Ow LG LM], Global, A, A_end, A_main)
    #([R W LG LM], Global, A_data, A_data_end, A_data)

loader_main:
    mov cra PC
    lea cra -1
    load cgp cra                 ; puts CGP_A in CGP
    lea cra -1
    load cra cra
    jalr cra cra                  ; jumps to A_main
    fail                        ; should never been reach, as A_main terminates the program
loader_end:

A:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
    #(E-[X Ow LG LM], Global, assert, assert_end, assert)
    #{9: ([R Ow LG LM], Global, B_ext, B_ext_end, B_ext_f)}
    #{9: ([R Ow LG LM], Global, C_ext, C_ext_end, C_ext_g)}
A_main:
    ;; b := 0; c := 0; call B.f with a bounded capability to b.
    store cgp 0
    mov ca0 cgp
    lea cgp 1
    store cgp 0
    geta ct0 ca0
    add ct1 ct0 1
    subseg ca0 ct0 ct1
    %fetch(0, ctp, ct0, ct1)
    %fetch(2, ct1, ct0, cs0)
    jalr cra ctp
    ;; assert(c == 0)
    load ct0 cgp
    mov ct1 0
    %assert_eq(1, ct2, ct3, ct4)
    ;; b := 42; call C.g with a bounded capability to c.
    mov ca0 cgp
    mov ca1 0
    lea cgp -1
    store cgp 42
    geta ct0 ca0
    add ct1 ct0 1
    subseg ca0 ct0 ct1
    %fetch(0, ctp, ct0, ct1)
    %fetch(3, ct1, ct0, cs0)
    jalr cra ctp
    ;; assert(b == 42)
    load ct0 cgp
    mov ct1 42
    %assert_eq(1, ct2, ct3, ct4)
    halt
A_end:

B:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc) ; import switcher
    ;; no import
B_f:
    store ca0 7
    store csp ca0
    jalr cnull cra
B_end:

C:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc) ; import switcher
    ;; no import
C_g:
    store ca0 9
    jalr cnull cra
C_end:

A_data:
    #0
    #0
A_data_end:

B_data:
    #0x0
B_data_end:

C_data:
    #0x0
C_data_end:

;; export table compartment A -> does not export any entry points
A_ext:
    #([X Ow LG LM], Global, A, A_end, A)                 ; PCC
    #([R W LG LM], Global, A_data, A_data_end, A_data)   ; CGP
A_ext_end:

;; export table compartment B -> exports B_f
B_ext:
    #([X Ow LG LM], Global, B, B_end, B)                 ; PCC
    #([R W LG LM], Global, B_data, B_data_end, B_data)   ; CGP
B_ext_f: #(((B_f - B) << 3) || 1)                         ; offset_f
B_ext_end:

;; export table compartment C -> exports C_g
C_ext:
    #([X Ow LG LM], Global, C, C_end, C)                 ; PCC
    #([R W LG LM], Global, C_data, C_data_end, C_data)   ; CGP
C_ext_g: #(((C_g - C) << 3) || 1)                         ; offset_g
C_ext_end:

;; Assert library from theories/case_studies/macros/assert.v.
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


;; Concatenate this file at the end of any example that require the switcher
%define ECOMPARTMENTFAIL -1
%define ENOTENOUGHTRUSTEDSTACK -141

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
    jmp (switcher_zero_stk_end_post - switcher_zero_stk_loop_post - 1)  ;
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
    mov ca0 ENOTENOUGHTRUSTEDSTACK
    mov ca1 0
    jmp (switcher_callee_dead_zeros - &CURRENT_ADDR)
switcher_force_unwind:
    mov ca0 ECOMPARTMENTFAIL
    mov ca1 0
    jmp (switcher_after_compartment_call - &CURRENT_ADDR)
switcher_end:
