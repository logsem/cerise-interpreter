main:
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    #{9: (R, Global, ext_adv, ext_adv_end, ext_adv+2)}            ; import ext

main_f:
    mov cra PC
    lea cra -1
    load ct1 cra                  ; ct1 := entry point
    lea cra -1
    load cra cra                  ; cra := switcher
    store csp r22
    lea csp -1
    jalr cra cra

    mov cra PC
    lea cra -9
    load ct1 cra                  ; ct1 := entry point
    lea cra -1
    load cra cra                  ; cra := switcher
    jalr cra cra

    halt
main_end:

adv:
    mov ca0 1
    mov ca1 42
    mov ca2 43
    jalr cra cra
adv_end:

data_main:
    #0xFFFF
data_main_end:

data_adv:
    #0x0
data_adv_end:

;; export table compartment c
ext_adv:
    #([R X], Global, adv, adv_end, adv)                 ; PCC
    #([R W], Global, data_adv, data_adv_end, data_adv)  ; CGP
    #00                                                 ; offset + args
ext_adv_end:

;; Concatenate this file at the end of any example that require the switcher
switcher:
    #[SU, Global, 9, 10, 9]
switcher_cc:
    store csp cs0
    lea csp -1
    store csp cs1
    lea csp -1
    store csp cra
    lea csp -1
    store csp cgp
    getp ct2 csp
    mov ctp [R W WL]
    sub ct2 ct2 ctp
    jnz ct2 2
    jmp 2
    fail
    movsr ct2 mtdc
    lea ct2 -1
    store ct2 csp
    movsr mtdc ct2
    geta cs0 csp
    getb cs1 csp
    subseg csp cs1 cs0
switcher_zero_stk_init_pre:
    sub cs0 cs1 cs0
    mov cs1 csp
    lea cs1 cs0
switcher_zero_stk_loop_pre:
    jnz cs0 2
    jmp (switcher_zero_stk_end_pre - switcher_zero_stk_loop_pre - 1)
    store cs1 0
    lea cs1 1
    add cs0 cs0 1
switcher_zero_stk_loop_end_pre:
    jmp (switcher_zero_stk_loop_pre - switcher_zero_stk_loop_end_pre)
switcher_zero_stk_end_pre:
    lea csp -1
    getb cs1 PC
    geta cs0 PC
    sub cs1 cs1 cs0
    mov cs0 PC
    lea cs0 cs1
    lea cs0 -2
    load cs0 cs0
    unseal ct1 cs0 ct1
    load cs0 ct1
    rem ct2 cs0 10
    sub cs0 cs0 ct2
    div cs0 cs0 10
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
    movsr ctp mtdc
    load csp ctp
    lea ctp 1
    movsr mtdc ctp
    load cgp csp
    lea csp 1
    load ca2 csp
    lea csp 1
    load cs1 csp
    lea csp 1
    load cs0 csp
    lea csp 1
switcher_zero_stk_init_post:
    geta ct0 csp
    getb ct1 csp
    sub ct0 ct1 ct0
    mov ct1 csp
    lea ct1 ct0
switcher_zero_stk_loop_post:
    jnz ct0 2
    jmp (switcher_zero_stk_end_post - switcher_zero_stk_loop_post - 1)  ;
    store ct1 0
    lea ct1 1
    add ct0 ct0 1
switcher_zero_stk_loop_end_post:
    jmp (switcher_zero_stk_loop_post - switcher_zero_stk_loop_end_post)
switcher_zero_stk_end_post:
    mov cra ca2
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
    jalr cra cra
switcher_end:
