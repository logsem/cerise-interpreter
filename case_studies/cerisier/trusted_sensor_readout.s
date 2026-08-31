;; Trusted sensor readout, faithfully instantiated from the pinned Rocq artifact
;; at cerisier 57ed584ae17eed308ae0fa554cf0dde9843112c1:
;;   theories/case_studies/memory_readout/
;;     {trusted_memory_readout_code,trusted_memory_readout_adequacy}.v
;; with assert_reg_instrs and assert_subroutine_instrs expanded from
;; theories/case_studies/macros/{macros,assert}.v.
;;
;; Rocq's r_t0..r_t5 aliases map directly to r0..r5.  The verifier occupies
;; [0,39), the 33-instruction adversary witness [39,72), the sensor and its data
;; [72,116), the client and its data [116,163), one-word sensor MMIO [163,164),
;; the assertion library [164,179), and its separate one-entry link table
;; [179,180).  The two constants are derived from staged fresh EInit executions.

%define TS_SENSOR_HASH 7182681053998082077296997438663194947164
%define TS_CLIENT_HASH 7795339977280813461738391276462452401620

;; trusted_memory_readout_main_code 0: 37 instructions and two data words.
verifier_start:
    ;; trusted_memory_readout_main_code_init0
    mov r1 pc
    lea r1 (verifier_callback-verifier_start)
    restrict r1 E
    jmp r0

verifier_callback:
    ;; trusted_memory_readout_main_code_callback0
    mov r5 pc
    lea r5 (verifier_fail-verifier_callback)

    getotype r2 r0
    sub r3 r2 -1
    mov r4 pc
    lea r4 4
    jnz r4 r3
    jmp r5

    estoreid r3 r2
    sub r3 r3 TS_CLIENT_HASH
    jnz r5 r3

    unseal r1 r1 r0
    mov r0 pc
    lea r0 3
    jmp r1

    mov r1 r5
    mov r4 r2
    mov r5 42
    lea r1 1
    load r1 r1

    ;; assert_reg_instrs 0 r1
    lea r1 0
    load r1 r1
    mov r2 r0
    mov r0 pc
    lea r0 3
    jmp r1
    mov r0 r2
    mov r1 0
    mov r2 0

    mov r0 0
    mov r3 0
    halt

verifier_fail:
    fail

verifier_link_cap:
    #(RO, assert_link, assert_link_end, assert_link)
verifier_data_cap:
    #(RWX, verifier_start, verifier_end, verifier_link_cap)
verifier_end:

;; Concrete witness for the arbitrary adversary in the adequacy theorem.  It
;; destroys every broad authority before the relevant EInit or IsUnique check.
adversary_start:
    mov r31 r1
    mov r10 pc
    mov r11 pc
    mov r12 pc
mmio_authority:
    mov r13 pc
    subseg pc adversary_start adversary_end

    subseg r10 sensor_start sensor_end
    restrict r10 RX

    subseg r0 sensor_data sensor_data_end
    lea r0 (sensor_data-adversary_start)
    restrict r0 RW

    subseg r11 client_start client_end
    restrict r11 RX

    subseg r12 client_data client_data_end
    restrict r12 RW

    subseg r13 sensor_mmio sensor_mmio_end
    lea r13 (sensor_mmio-mmio_authority)
    restrict r13 RW

    einit r10 r0

    mov r1 r13
    mov r13 0
sensor_call:
    mov r0 pc
    lea r0 (after_sensor_init-sensor_call)
    restrict r0 E
    jmp r10

after_sensor_init:
    einit r11 r12

client_call:
    mov r0 pc
    lea r0 (after_client_init-client_call)
    restrict r0 E
    jmp r11

after_client_init:
    mov r0 r1
    mov r1 r2
    jmp r31
adversary_end:

;; sensor_code: reserved data-capability cell, exact 32-instruction initializer,
;; exact eight-instruction read entry, and the paper-defined fail instruction.
sensor_start:
    #0
sensor_init:
    mov r2 pc
    lea r2 (sensor_fail-sensor_init)

    getwtype r3 r1
    sub r3 r3 Cap
    jnz r2 r3

    getp r3 r1
    sub r3 r3 RW
    jnz r2 r3

    isunique r3 r1
    sub r3 1 r3
    jnz r2 r3

    store r1 21

    mov r3 r2
    lea r3 (sensor_start-sensor_fail)
    load r3 r3
    getb r4 r3
    geta r5 r3
    sub r4 r4 r5
    jnz r2 r4

    lea r3 1
    store r3 r1

    lea r3 -1
    load r1 r3
    lea r1 1

    lea r2 (sensor_read-sensor_fail)
    restrict r2 E
    seal r2 r1 r2

    geta r3 r1
    gete r4 r1
    subseg r1 r3 r4
    restrict r1 U

    jmp r0

sensor_read:
    mov r1 pc
    lea r1 (sensor_start-sensor_read)
    load r1 r1
    lea r1 1
    load r1 r1
    load r2 r1
    geta r1 r1
    jmp r0

sensor_fail:
    fail
sensor_end:

sensor_data:
    #0
    #0
sensor_data_end:

;; client_code: reserved data-capability cell, exact 26-instruction initializer,
;; exact 17-instruction use routine, and the paper-defined fail instruction.
client_start:
    #0
client_init:
    mov r3 pc
    lea r3 (client_fail-client_init)

    unseal r2 r1 r2
    geta r4 r1
    estoreid r1 r4
    sub r1 r1 TS_SENSOR_HASH
    jnz r3 r1

    lea r3 (client_start-client_fail)
    load r1 r3
    getb r4 r1
    geta r5 r1
    sub r4 r4 r5
    lea r1 r4

    lea r1 1
    store r1 r2

    lea r1 -1
    load r2 r1
    lea r2 1

    lea r3 (client_use-client_start)
    restrict r3 E
    seal r1 r2 r3

    geta r3 r2
    gete r4 r2
    subseg r2 r3 r4
    restrict r2 U

    jmp r0

client_use:
    mov r1 pc
    lea r1 (client_start-client_use)
    load r1 r1
    getb r2 r1
    geta r3 r1
    sub r2 r2 r3
    lea r1 r2

    lea r1 1
    load r1 r1

    mov r3 r0
    mov r0 pc
    lea r0 3
    jmp r1

    add r2 r2 r2
    mov r0 r3
    mov r3 0
    jmp r0

client_fail:
    fail
client_end:

client_data:
    #0
    #0
client_data_end:

sensor_mmio:
    #0
sensor_mmio_end:

;; Exact 13-instruction assert_subroutine_instrs, followed by its capability
;; and flag.  A mismatch records 1 in the flag and returns to the verifier.
assert_start:
    sub r4 r4 r5
    mov r5 pc
    lea r5 6
    jnz r5 r4
    mov r4 0
    mov r5 0
    jmp r0
assert_failure:
    lea r5 6
    load r5 r5
    store r5 1
    mov r4 0
    mov r5 0
    jmp r0
assert_cap:
    #(RW, assert_flag, assert_end, assert_flag)
assert_flag:
    #0
assert_end:

assert_link:
    #(E, assert_start, assert_end, assert_start)
assert_link_end:
