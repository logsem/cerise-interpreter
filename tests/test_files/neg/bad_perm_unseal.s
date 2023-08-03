    restrict r0 S
    seal r1 r0 pc
    unseal r1 r0 r1             ; FAIL: attempt to unseal without the (U)nseal permission
    halt
