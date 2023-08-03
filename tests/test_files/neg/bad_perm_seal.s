    restrict r0 U
    seal r1 r0 pc               ; FAIL: attempt to seal without the (S)eal permission
    halt
