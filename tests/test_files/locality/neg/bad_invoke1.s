    mov r1 pc
    subseg r1 7 15
    lea r1 7
    mov r2 r1
    restrict r1 (RX, Global)
    restrict r2 (RW, Global)
    invoke r1 r2                ; FAIL: attempt to invoke non-sealed capabilities
    halt
