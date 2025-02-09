code:
    mov r0 pc                   ; r0 Global

    restrict r0 ([XSR Ow LG LM], GLOBAL)          ; Global can be restricted to global
    restrict r0 ([XSR Ow LG LM], LOCAL)           ; Global can be restricted to local
    restrict r0 ([XSR Ow LG LM], LOCAL)           ; Local can be restricted to local

    halt
data:
