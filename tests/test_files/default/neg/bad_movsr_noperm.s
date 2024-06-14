    movsr r1 MTDC
    restrict PC [R X]
    movsr r1 MTDC               ; FAIL: attempt to use movsr without SR permission in PC
    halt
