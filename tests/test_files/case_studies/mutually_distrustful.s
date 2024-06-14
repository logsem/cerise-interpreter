    ;; TODO Something that could be cool !!
    ;; is to randomly generate code for B_f and C_g,
    ;; and run the tests for all generated code,
    ;; and have an assert flag in A,
    ;; and verify that for any executions, the flag does not change


    ;; main is just a very basic loader that jumps to the main compartment A
    ;; and terminates the machine
loader:
    #([R X], Global, A, A_end, A+3)                 ; PCC_A + main
    #([R W], Global, A_data, A_data_end, A_data)  ; CGP_A

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
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    ;; imports B_f and C_g
    #{9: ([R], Global, B_ext, B_ext_end, B_ext_f)} ; import switcher
    #{9: ([R], Global, C_ext, C_ext_end, C_ext_g)} ; import switcher
A_main:
    ;; store PCC_A on the stack, will be used to fetch imported entry point
    mov ct1 PC
    lea ct1 -3
    store csp ct1
    lea csp -1

    ;; put shared data in argument register ca0
    load ca0 cgp

    ;; fetch switcher entry point in cra
    load cra ct1
    ;; fetch entry point B_f in ct1
    lea ct1 1
    load ct1 ct1
    ;; jump to B_f with A_data_shared as argument in ca0
    jalr cra cra

    ;; write 0x42 in the shared_data
    load ct1 cgp
    store ct1 0x42

    ;; fetch switcher entry point in cra
    mov ct1 csp
    load ct1 ct1                  ; ct1 := (RX,Global,A,A_end,A)
    load cra ct1
    ;; fetch entry point C_g in ct1
    lea ct1 2
    load ct1 ct1
    ;; jump to C_g with no arguments
    jalr cra cra

    ;; assert that shared_data still contains 0x42
    load ct1 cgp
    load ct1 ct1
    sub ct1 ct1 0x42
    jnz ct1 2
    jmp 2
    fail
    ;; assert didn't fail
    halt
A_main_end:
A_end:

B:
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    ;; no import
B_h:
    #0x00
    #0x00
    #0x00
B_f:
    store cgp ca0
    store csp ca0               ; tries to use the stack to pass the pointer
    mov ca0 0xFF
    jalr cra cra
B_end:

C:
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    ;; no import
C_h:
    #0x0
C_g:
    ;; could be any code here
    jalr cra cra
C_end:

A_data:
    #([R W], Global, A_data_shared, A_data_shared + 1, A_data_shared)
A_data_shared:
    #0xFF
A_data_end:

B_data:
    #0x0
B_data_end:

C_data:
    #0x0
C_data_end:

;; export table compartment A -> does not export any entry points
A_ext:
    #([R X], Global, A, A_end, A)                 ; PCC
    #([R W], Global, A_data, A_data_end, A_data)  ; CGP
A_ext_end:

;; export table compartment B -> exports B_f
B_ext:
    #([R X], Global, B, B_end, B)                 ; PCC
    #([R W], Global, B_data, B_data_end, B_data)  ; CGP
B_ext_f: #(10 * (B_f - B) + 1)                    ; offset_f
B_ext_h: #(10 * (B_f - B) + 0)                    ; offset_h
B_ext_end:

;; export table compartment C -> exports C_g
C_ext:
    #([R X], Global, C, C_end, C)                 ; PCC
    #([R W], Global, C_data, C_data_end, C_data)  ; CGP
C_ext_g: #(10 * (C_g - C) + 0)                    ; offset_g
C_ext_end:

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
