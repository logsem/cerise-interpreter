switcher:
    #[SU, Global, 9, 10, 9]

    ;; https://github.com/CHERIoT-Platform/llvm-project/blob/f39e8860b29668f986b11d29fa953c96a25373f1/llvm/lib/Target/RISCV/RISCVRegisterInfo.td#L98

    ;; cs0           -> scratch register
    ;; cs1           -> scratch register
    ;; ct1 / rentry  -> contains the sealed target capability
    ;; cra / rlink   -> contains the sentry back to caller's compartment
    ;; csp / rstk    -> contains the compartment's stack capability
    ;;
    ;; ct2           -> scratch register
    ;; ctp           -> scratch register
    ;; ca1           -> return value 1
    ;; ca2           -> return value 2
switcher_cc:
    ;; STEP 1: store content in compartment's stack
    store csp cs0
    lea csp -1
    store csp cs1
    lea csp -1
    store csp cra
    lea csp -1
    store csp cgp

    ;; STEP 2: verify csp contains a valid stack pointer
    ;; verify permissions
    getp ct2 csp
    mov ctp [R W WL]
    sub ct2 ct2 ctp
    jnz ct2 2
    jmp 2
    fail                        ; ct2 :=/= 0, ie. not the right permissions
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
    movsr ct2 mtdc
    lea ct2 -1
    store ct2 csp
    movsr mtdc ct2              ; NOTE: in the actual implementation,
                                ; mtdc is not updated, but the offset of the current
                                ; stack frame is stored directly in the tstack
                                ; Should I do that too ?

    ;; STEP 5: restrict bounds of the stack
    geta cs0 csp                ; cs0 := a
    getb cs1 csp                ; cs1 := b
    subseg csp cs1 cs0          ; csp := (p,g,b,a,a)

    ;; STEP 6: zero the callee's stack frame
    ;; TODO do we want the stack high water mark ?
    ;; if so, we would need to implement more special registers
switcher_zero_stk_init_pre:
    sub cs0 cs1 cs0             ; cs0 := b-a
    mov cs1 csp                 ; cs1 := (p,g,b,a,a)
    lea cs1 cs0                 ; cs1 := (p,g,b,a,b)
    ;; cs0: i := -(b-a)
switcher_zero_stk_loop_pre:
    jnz cs0 2                   ; if (i = 0) then (end of loop), otherwise continue
    jmp (switcher_zero_stk_end_pre - switcher_zero_stk_loop_pre - 1)  ;

    store cs1 0                 ; mem[b+i] := 0
    lea cs1 1                   ; cs1 := (p,g,b,a,b+i)
    add cs0 cs0 1               ; i := i + 1
switcher_zero_stk_loop_end_pre:
    jmp (switcher_zero_stk_loop_pre - switcher_zero_stk_loop_end_pre)
switcher_zero_stk_end_pre:
    lea csp -1

    ;; STEP 7: unseal the callee's entry point
    ; LoadCapPCC ......
    getb cs1 PC                 ; cs1 := b_cc
    geta cs0 PC                 ; cs0 := a_cc
    sub cs1 cs1 cs0             ; cs1 := b_cc - a_cc
    mov cs0 PC                  ; cs0 := (RX_SR, Global, b_cc, e_cc, a_cc+2)
    lea cs0 cs1                 ; cs0 := (RX_SR, Global, b_cc, e_cc, b_cc+2)
    lea cs0 -2                  ; cs0 := (RX_SR, Global, b_cc, e_cc, b_cc)
    load cs0 cs0                ; cs0 := #[SU, Global, 9, 10, 9]

    unseal ct1 cs0 ct1          ; ct1 := (RO, Global, b_ext, e_ext, a_ext)
    ; load entry point
    load cs0 ct1                ; cs0 := encodeEntry(offset, args)
    ;; encodeEntry(offset, args) = (10 x offset) + args
    ;; decodeEntry_args(z) = mod(10,z)
    ;; decodeEntry_offset(z) = (z - mod(10,z)) / 10

    ;; get the number of registers to zero in ct2
    rem ct2 cs0 10              ; ct2 := args
    ; TODO check that args < 7
    ;; get the offset
    sub cs0 cs0 ct2             ; encodeEntry(offset, args) - args
    div cs0 cs0 10              ; offset

    getb cgp ct1                ; cgp := b_ext
    geta cs1 ct1                ; cs1 := a_ext
    sub cs1 cgp cs1             ; cs1 := b_ext - a_ext
    lea ct1 cs1                 ; ct1 := (RO, Global, b_ext, e_ext, b_ext)
    load cra ct1                ; cra := PCC
    lea ct1 1                   ; ct1 := (RO, Global, b_ext, e_ext, b_ext+1)
    load cgp ct1                ; cgp := CGP
    ;; get the actual entry point
    lea cra cs0                 ; cra := PCC + offset

    ;; STEP 8: zero the unused arguments
    ;; we do not clear the used arguments
    add ct2 ct2 1
    jmp ct2
    mov r10 0                   ; ca0
    mov r11 0                   ; ca1
    mov r12 0                   ; ca2
    mov r13 0                   ; ca3
    mov r14 0                   ; ca4
    mov r15 0                   ; ca5
    mov r5 0                    ; ct0, stack argument

    mov r0 0
    ;; r1 / cra ---> contains PCC
    ;; r2 / csp ---> contains compartment's stack
    ;; r3 / cgp ---> contains CGP
    mov r4 0
    ;; r5 ---> register stack arguments
    mov r6 0
    mov r7 0
    mov r8 0
    mov r9 0
    ;; r10-15 ---> register arguments
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
    ;; r31 ---> contains trusted stack

    ;; STEP 9: jump to the callee's compartment
    jalr cra cra

    ;; STEP 10: pop topmost trusted stack frame
    movsr ctp mtdc
    ;; TODO make sure that there is a frame left in the trusted stack
    ;; restore stack pointer and update trusted stack
    load csp ctp
    lea ctp 1
    movsr mtdc ctp
    ;; spill the saved registers out
    load cgp csp
    lea csp 1
    load ca2 csp
    lea csp 1
    load cs1 csp
    lea csp 1
    load cs0 csp

    ;; zero the stack frame
    ;; TODO do we want the stack high water mark ?
switcher_zero_stk_init_post:
    geta ct0 csp                ; r20 := a
    getb ct1 csp                ; ctp := b
    sub ct0 ct1 ct0             ; ct0 := b-a
    mov ct1 csp                 ; ct1 := (p,g,b,a,a)
    lea ct1 ct0                 ; ct1 := (p,g,b,a,b)
    ;; ct0: i := -(b-a)
switcher_zero_stk_loop_post:
    jnz ct0 2                   ; if (i = 0) then (end of loop), otherwise continue
    jmp (switcher_zero_stk_end_post - switcher_zero_stk_loop_post - 1)  ;

    store ct1 0                 ; mem[b+i] := 0
    lea ct1 1                   ; ct1 := (p,g,b,a,b+i)
    add ct0 ct0 1               ; i := i + 1
switcher_zero_stk_loop_end_post:
    jmp (switcher_zero_stk_loop_post - switcher_zero_stk_loop_end_post)
switcher_zero_stk_end_post:

    mov cra ca2

    ;; STEP 11: zero unused registers
    mov r0 0
    ;; r1  / cra ---> saved and restored on trusted stack
    ;; r2  / csp ---> compartment's stack
    ;; r3  / cgp ---> saved and restored on trusted stack
    mov r4 0
    mov r5 0
    mov r6 0
    mov r7 0
    ;; r8 / cs0 ---> saved and restored on trusted stack
    ;; r9 / cs1 ---> saved and restored on trusted stack
    ;; r10 / ca1 ---> first return value
    ;; r11 / ca2 ---> second return value
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
    ;; r31 / mtdc ---> trusted stack
    jalr cra cra

switcher_end:
