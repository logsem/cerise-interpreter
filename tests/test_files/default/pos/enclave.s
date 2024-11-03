boot:
    mov r1 pc
    mov r0 0
    mov r31 0

    ;; subseg the boot
    getb r2 pc
    mov r3 (tc_start-boot)
    subseg pc r2 r3

    ;; prepare tc enclave
    mov r2 (tc_start-boot)
    mov r3 (tc_data-boot)
    subseg r1 r2 r3
    restrict r1 (RX, Global)

    ;; init enclave
    einit r0 r1
    mov r31 pc
    lea r31 3
    jmp r0
    getotype r2 r1
    estoreid r3 r2 r31
    load r3 r31

    ;; deinit enclave
    mov r31 pc
    lea r31 3
    jmp r5
    halt

tc_start:
    #(RW, Global, tc_data, tc_end, tc_data)
tc_main:
    mov r0 pc
    lea r0 (-1)

    mov r5 r0
    lea r5 (tc_deinit-tc_start)
    restrict r5 (E, Global)

    load r1 r0
    load r1 r1
    mov r0 pc
    geta r3 r0
    sub r3 0 r3
    lea r0 r3
    lea r0 42
    restrict r0 (O, Global)
    seal r1 r1 r0

    jmp r31
tc_deinit:
    mov r0 pc
    lea r0 (tc_start-tc_deinit)
    load r1 r0
    load r1 r1
    edeinit r1 r1
    jmp r31
tc_data:
    #0
    #1
    #0
tc_end:
