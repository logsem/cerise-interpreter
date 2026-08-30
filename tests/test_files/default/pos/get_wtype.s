    mov r1 pc
    mov r2 12
    lea r0 10
    seal r3 r0 pc

    getwtype r0 r0
    getwtype r1 r1
    getwtype r2 r2
    getwtype r3 r3

    sub r0 r0 SealRange
    sub r1 r1 Cap
    sub r2 r2 Int
    sub r3 r3 Sealed
    halt
