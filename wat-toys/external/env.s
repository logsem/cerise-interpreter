.text
    mov r10 r0                ; fetch data capanbility
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
    #(RO, Global, 19, 22, 19)       ; function closure adv
    #{2: (RW, Global, 16, 19, 16)}  ; sealed to global
    $main.g
    $_Common.link_tbl
    #0
    #0
    ;; Global h
    #3
    #0
    #1
    ;; Function adv
    #{4: (RX, Global, 0, 10, 0)}
    #{4: (RO, Global, 10, 14, 10)}
    #0
.imports
.exports
    $env.h: .data 1
    $env.adv: .data 0
.init
