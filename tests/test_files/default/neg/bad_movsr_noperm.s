    readsr r1 MTDC
    restrict PC [X Ow LG LM]
    readsr r1 MTDC               ; FAIL: attempt to use readsr without SR permission in PC
    halt
