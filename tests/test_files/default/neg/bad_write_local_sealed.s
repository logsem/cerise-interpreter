    mov r1 pc
    restrict r1 (O, LOCAL)
    seal r1 r0 r1 ;; try to write a sealed local cap without WL permission
    store pc r1
	halt
