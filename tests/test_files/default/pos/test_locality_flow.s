code:
    mov r0 pc                   ; r0 Global

    restrict r0 ([R X SR], GLOBAL)          ; Global can be restricted to global
    restrict r0 ([R X SR], LOCAL)           ; Global can be restricted to local
    restrict r0 ([R X SR], LOCAL)           ; Local can be restricted to local

    halt
data:
