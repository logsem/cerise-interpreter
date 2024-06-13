main:
    #{0: ([R X SR], Global, switcher, switcher_end, switcher_cc)} ; import switcher
    #{9: (R, Global, ext_adv, ext_adv_end, ext_adv+2)}                 ; import ext

main_f:
    mov r0 PC
    lea r0 -1
    load r1 r0                  ; r1 := entry point
    lea r0 -1
    load r0 r0                  ; r0 := switcher
    store stk r22
    lea stk -1
    jalr r0 r0

    mov r0 PC
    lea r0 -9
    load r1 r0                  ; r1 := entry point
    lea r0 -1
    load r0 r0                  ; r0 := switcher
    jalr r0 r0

    halt
main_end:

adv:
    mov r21 1
    mov r22 42
    mov r23 43
    jalr r0 r0
adv_end:

switcher:
    #[SU, Global, 9, 10, 9]

    ;; cs0 := r20
    ;; cs1 := r21
    ;; ct1 / rentry := r1
    ;; cra / rlink := r0
    ;; csp / rstk  := stk
switcher_cc:
    ;; STEP 1: store content in compartment's stack
    store stk r20
    lea stk -1
    store stk r21
    lea stk -1
    store stk r0
    lea stk -1
    store stk cgp

    ;; STEP 2: verify stk contains a valid stack pointer
    ;; verify permissions
    getp r20 stk
    mov r21 [R W WL]
    sub r20 r20 r21
    jnz r20 2
    jmp 2
    fail                        ; r20 :=/= 0, ie. not the right permissions
    ;; SKIP verify alignments

    ;; STEP 3: verify valid trusted stack
    ;; TODO SKIP
    ;; movsr r21 mtdc
    ;; geta r20 r21
    ;; gete r21 r21
    ;; add r20 r20 1
    ;; lt r20 r20 r21              ; r20 contains 0 if a >= e
    ;; jnz r20 2
    ;; fail                        ; r20 := 0, i.e, the stack does not have enough space

    ;; STEP 4: prepare the tstack frame
    movsr r21 mtdc
    lea r21 -1
    store r21 stk
    movsr mtdc r21

    ;; STEP 5: restrict bounds of the stack
    geta r20 stk                ; r20 := a
    getb r21 stk                ; r21 := b
    subseg stk r21 r20          ; stk := (p,g,b,a,a)

    ;; STEP 6: zero the callee's stack frame
zero_stk_init:
    sub r20 r21 r20             ; r20 := b-a
    mov r21 stk                 ; r21 := (p,g,b,a,a)
    lea r21 r20                 ; r21 := (p,g,b,a,b)
    ;; r20: i := -(b-a)
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

    ;; STEP 7: unseal the callee's entry point
    ; LoadCapPCC ......
    getb r21 PC                 ; b_cc
    geta r20 PC                 ; a_cc
    sub r21 r21 r20             ; b_cc - a_cc
    mov r20 PC                  ; r20 := (RX_SR, Global, b_cc, e_cc, a_cc+2)
    lea r20 r21                 ; r20 := (RX_SR, Global, b_cc, e_cc, b_cc+2)
    lea r20 -2                  ; r20 := (RX_SR, Global, b_cc, e_cc, b_cc)
    load r20 r20                ; r20 := #[SU, Global, 9, 10, 9]

    unseal r1 r20 r1            ; r1 := (RO, Global, b_ext, e_ext, a_ext)
    ; load entry point offset
    load r20 r1                 ; r20 := encodeEntry(offset, args)
    ;; TODO for simplicity, we say that args always = 6
    ;; encodeEntry(offset, args) = offset
    ;; decodeEntry(z) = (offset, 6)
    getb cgp r1                 ; cgp := b_ext
    geta r21 r1                 ; r21 := a_ext
    sub r21 cgp r21             ; r21 := b_ext - a_ext
    lea r1 r21                  ; r1 := (RO, Global, b_ext, e_ext, b_ext)
    load r0 r1                  ; r0 := PCC
    lea r1 1                    ; r1 := (RO, Global, b_ext, e_ext, b_ext+1)
    load cgp r1                 ; cgp := CGP
    ;; get the actual entry point
    lea r0 r20                  ; r0 := PCC + offset

    ;; STEP 8: zero the unused arguments
    ;; r0 ---> contains PCC
    mov r1 0
    mov r2 0
    mov r3 0
    mov r4 0
    ;; r5 ---> contains CGP
    ;; r6 ---> contains stack arguments
    mov r7 0
    mov r8 0
    mov r9 0
    ;; r10-15 ---> contains register arguments
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
    ;; r30 ---> contains compartment's stack
    ;; r31 ---> contains trusted stack

    ;; STEP 9: jump to the callee's compartment
    jalr r0 r0
    ;; STEP 10: pop topmost trusted stack frame
    movsr r21 mtdc
    ;; TODO make sure that there is a frame left in the trusted stack
    ;; restore stack pointer and update trusted stack
    load stk r21
    lea r21 1
    movsr mtdc r21
    ;; spill the saved registers out
    load cgp stk
    lea stk 1
    load r24 stk
    lea stk 1
    load r21 stk
    lea stk 1
    load r20 stk
    lea stk 1

    ;; TODO zero the stack frame
zero_stk_init2:
    geta r25 stk                ; r20 := a
    getb r26 stk                ; r21 := b
    sub r25 r26 r25             ; r25 := b-a
    mov r26 stk                 ; r26 := (p,g,b,a,a)
    lea r26 r25                 ; r26 := (p,g,b,a,b)
    ;; r25: i := -(b-a)
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

    ;; STEP 11: zero unused registers
    ;; r0  / cra ---> saved and restored on trusted stack
    mov r1 0
    mov r2 0
    mov r3 0
    mov r4 0
    ;; r5  / cgp ---> saved and restored on trusted stack
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
    ;; r20 / cs0 ---> saved and restored on trusted stack
    ;; r21 / cs1 ---> saved and restored on trusted stack
    ;; r22 / ca1 ---> first return value
    ;; r23 / ca2 ---> second return value
    mov r24 0
    mov r25 0
    mov r26 0
    mov r27 0
    mov r28 0
    mov r29 0
    ;; r30 / stk ---> contains compartment's stack
    ;; r31 / tsk ---> contains trusted stack
    jalr r0 r0
switcher_end:

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
    #0                                                  ; offset
ext_adv_end:
