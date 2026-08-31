    mov r1 pc
    subseg r1 9 15
    lea r1 9
    mov r2 r1
    restrict r1 (RX, Global)
    restrict r2 (RW, Global)
    seal r1 r0 r1
    seal r2 r0 r2
    invoke r1 r2
    halt
