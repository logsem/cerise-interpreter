    mov r1 PC
    lea r1 5
    load r1 r1
    subseg r1 10 11
    jmp r2
    #(RW,Global,9,12,9)
    lea r1 1
    store r1 42
    halt
    #0
    #0
    #0
