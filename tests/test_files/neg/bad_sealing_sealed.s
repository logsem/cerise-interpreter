    seal r1 r0 pc
    seal r1 r0 r1               ; FAIL: attempt to seal a sealed capability, but it is not sealable
    halt
