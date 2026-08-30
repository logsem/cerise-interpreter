boot:
    mov r1 pc
    mov r0 pc
    mov r31 0

    ;; subseg the boot
    getb r2 pc
    mov r3 (tc_start-boot)
    subseg pc r2 r3

    ;; prepare tc enclave
    mov r2 (tc_start-boot)
    mov r3 (tc_data-boot)
    subseg r1 r2 r3
    lea r1 tc_start
    store r1 0
    lea r1 (tc_main-tc_start)
    restrict r1 RX

    mov r2 (tc_data-boot)
    mov r3 (tc_end-boot)
    subseg r0 r2 r3
    lea r0 (tc_data-boot-1)
    restrict r0 RW

    ;; init enclave
    einit r1 r0
    mov r0 r1
    mov r31 pc
    lea r31 3
    jmp r0
    getotype r2 r1
    estoreid r3 r2

    ;; deinit enclave
    mov r31 pc
    lea r31 3
    jmp r5
    halt

tc_start:
    #(RW, tc_data, tc_end, tc_data)
tc_main:
    mov r0 pc
    lea r0 (-1)

    mov r5 r0
    lea r5 (tc_deinit-tc_start)
    restrict r5 E

    load r1 r0
    load r1 r1
    mov r0 pc
    geta r3 r0
    sub r3 0 r3
    lea r0 r3
    lea r0 42
    restrict r0 O
    seal r1 r1 r0

    jmp r31
tc_deinit:
    mov r0 pc
    lea r0 (tc_start-tc_deinit)
    load r1 r0
    load r1 r1
    edeinit r1
    jmp r31
tc_data:
    #0
    #1
    #0
tc_end:
