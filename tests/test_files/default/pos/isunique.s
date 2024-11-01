    mov r1 pc
    mov ddc 0

    mov r2 0
    geta r3 pc
    subseg r1 r2 r3
    isunique r5 r1

    geta r2 pc
    gete r3 pc
    subseg pc r2 r3
    isunique r6 r1

    store pc r1
    isunique r7 r1

    halt
