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
    mov r0 PC
    lea r0 -1
    load cgp r0                 ; puts CGP_A in CGP
    lea r0 -1
    load r0 r0
    jalr r0 r0                  ; jumps to A_main
    fail                        ; should never been reach, as A_main terminates the program
loader_end:

A:
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    ;; imports B_f and C_g
    #{9: ([R], Global, B_ext, B_ext_end, B_ext_f)} ; import switcher
    #{9: ([R], Global, C_ext, C_ext_end, C_ext_g)} ; import switcher
A_main:
    ;; store PCC_A on the stack, will be used to fetch imported entry point
    mov r1 PC
    lea r1 -3
    store stk r1
    lea stk -1

    ;; put shared data in argument register r10
    load r10 cgp

    ;; fetch switcher entry point in r0
    load r0 r1
    ;; fetch entry point B_f in r1
    lea r1 1
    load r1 r1
    ;; jump to B_f with A_data_shared as argument in r10
    jalr r0 r0

    ;; write 0x42 in the shared_data
    load r1 cgp
    store r1 0x42

    ;; fetch switcher entry point in r0
    mov r1 stk
    load r1 r1                  ; r1 := (RX,Global,A,A_end,A)
    load r0 r1
    ;; fetch entry point C_g in r1
    lea r1 2
    load r1 r1
    ;; jump to C_g with no arguments
    jalr r0 r0

    ;; assert that shared_data still contains 0x42
    load r1 cgp
    load r1 r1
    sub r1 r1 0x42
    jnz r1 2
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
    store cgp r10
    store stk r10               ; tries to use the stack to pass the pointer
    mov r22 0xFF
    jalr r0 r0
B_end:

C:
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    ;; no import
C_h:
    #0x0
C_g:
    ;; could be any code here
    jalr r0 r0
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
B_ext_f: #(B_f - B)                               ; offset_f
B_ext_h: #(B_h - B)                               ; offset_h
B_ext_end:

;; export table compartment C -> exports C_g
C_ext:
    #([R X], Global, C, C_end, C)                 ; PCC
    #([R W], Global, C_data, C_data_end, C_data)  ; CGP
C_ext_g: #(C_g - C)                               ; offset_g
C_ext_end:

;;; NOTE The switcher is copy-paste from switcher/switcher.s
switcher:
    #[SU, Global, 9, 10, 9]
switcher_cc:
    store stk r20
    lea stk -1
    store stk r21
    lea stk -1
    store stk r0
    lea stk -1
    store stk cgp
    getp r20 stk
    mov r21 [R W WL]
    sub r20 r20 r21
    jnz r20 2
    jmp 2
    fail                        ; r20 :=/= 0, ie. not the right permissions
    movsr r21 mtdc
    lea r21 -1
    store r21 stk
    movsr mtdc r21
    geta r20 stk                ; r20 := a
    getb r21 stk                ; r21 := b
    subseg stk r21 r20          ; stk := (p,g,b,a,a)
zero_stk_init:
    sub r20 r21 r20             ; r20 := b-a
    mov r21 stk                 ; r21 := (p,g,b,a,a)
    lea r21 r20                 ; r21 := (p,g,b,a,b)
zero_stk_loop:
    jnz r20 2                   ; if (i = 0) then (end of loop), otherwise continue
    jmp (zero_stk_end - zero_stk_loop - 1)  ;
    store r21 0                 ; mem[b+i] := 0
    lea r21 1                   ; r21 := (p,g,b,a,b+i)
    add r20 r20 1               ; i := i + 1
zero_stk_loopp:
    jmp (zero_stk_loop - zero_stk_loopp)
zero_stk_end:
    lea stk -1
    getb r21 PC                 ; b_cc
    geta r20 PC                 ; a_cc
    sub r21 r21 r20             ; b_cc - a_cc
    mov r20 PC                  ; r20 := (RX_SR, Global, b_cc, e_cc, a_cc+2)
    lea r20 r21                 ; r20 := (RX_SR, Global, b_cc, e_cc, b_cc+2)
    lea r20 -2                  ; r20 := (RX_SR, Global, b_cc, e_cc, b_cc)
    load r20 r20                ; r20 := #[SU, Global, 9, 10, 9]
    unseal r1 r20 r1            ; r1 := (RO, Global, b_ext, e_ext, a_ext)
    load r20 r1                 ; r20 := encodeEntry(offset, args)
    getb cgp r1                 ; cgp := b_ext
    geta r21 r1                 ; r21 := a_ext
    sub r21 cgp r21             ; r21 := b_ext - a_ext
    lea r1 r21                  ; r1 := (RO, Global, b_ext, e_ext, b_ext)
    load r0 r1                  ; r0 := PCC
    lea r1 1                    ; r1 := (RO, Global, b_ext, e_ext, b_ext+1)
    load cgp r1                 ; cgp := CGP
    lea r0 r20                  ; r0 := PCC + offset
    mov r1 0
    mov r2 0
    mov r3 0
    mov r4 0
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
    jalr r0 r0
    movsr r21 mtdc
    load stk r21
    lea r21 1
    movsr mtdc r21
    load cgp stk
    lea stk 1
    load r24 stk
    lea stk 1
    load r21 stk
    lea stk 1
    load r20 stk
    lea stk 1
zero_stk_init2:
    geta r25 stk                ; r20 := a
    getb r26 stk                ; r21 := b
    sub r25 r26 r25             ; r25 := b-a
    mov r26 stk                 ; r26 := (p,g,b,a,a)
    lea r26 r25                 ; r26 := (p,g,b,a,b)
zero_stk_loop2:
    jnz r25 2                   ; if (i = 0) then (end of loop), otherwise continue
    jmp (zero_stk_end2 - zero_stk_loop2 - 1)  ;
    store r26 0                 ; mem[b+i] := 0
    lea r26 1                   ; r26 := (p,g,b,a,b+i)
    add r25 r25 1               ; i := i + 1
zero_stk_loopp2:
    jmp (zero_stk_loop2 - zero_stk_loopp2)
zero_stk_end2:
    mov r0 r24                  ; mov cra, ca2
    mov r1 0
    mov r2 0
    mov r3 0
    mov r4 0
    mov r6 0
    mov r7 0
    mov r8 0
    mov r9 0
    mov r10 0
    mov r11 0
    mov r12 0
    mov r13 0
    mov r14 0
    mov r15 0
    mov r16 0
    mov r17 0
    mov r18 0
    mov r19 0
    mov r24 0
    mov r25 0
    mov r26 0
    mov r27 0
    mov r28 0
    mov r29 0
    jalr r0 r0
switcher_end:
