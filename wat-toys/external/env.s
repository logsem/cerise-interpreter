.text
    #(RO, Global, 13, 17, 13)
    mov r10 pc
    lea r10 -1
    load r10 r10                ; fetch data capanbility
    lea r10 2
    load r1 r10                 ; fetch capability to global Main.g
    lea r10 1
    load r10 r10                ; fetch common.link_tbl cap
    lea r10 5
    load r10 r10                ; fetch store_global cap
    mov r2 31
    loadU r0 stk -3
    jmp r10

.data
    #(RO, Global, 22, 24, 22)       ; function closure adv
    #{1: (RW, Global, 19, 22, 19)}  ; sealed to global
    $main.g
    $_Common.link_tbl
    #0
    #0
    ;; Global h
    #3
    #0
    #1
    ;; Function adv
    #(E, Global, 0, 13, 1)
    #0

.symtab
    $env.h: .data 1
    $env.adv: .data 0
