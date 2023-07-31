    subseg r0 0 2
    mov r1 r0
    restrict r0 S
    restrict r1 U
    seal r2 r0 pc
    seal r3 r0 r1
    unseal r2 r1 r2
    unseal r3 r1 r3
    halt
