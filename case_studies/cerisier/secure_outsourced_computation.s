;; Secure outsourced computation (SOC).
;;
;; Faithful executable instantiation of the pinned Rocq case study at
;; 57ed584ae17eed308ae0fa554cf0dde9843112c1:
;;   theories/case_studies/soc/{soc_code,soc_adequacy}.v
;; with assert_reg_instrs and assert_subroutine_instrs expanded from
;; theories/case_studies/macros/{macros,assert}.v.
;;
;; Rocq's r_t0..r_t5 aliases map directly to r0..r5.  The verifier occupies
;; [0,39), the adversary executable [39,53), the enclave and its data [53,76),
;; the assertion library [76,91), and its one-entry link table [91,92).
;; SOC_HASH is the concrete compositional identity obtained by assembling this
;; fixed layout with SOC_HASH = 0, executing the ten setup steps and EInit, and
;; reading enclave-table entry 0 through Machine_view.

%define SOC_HASH 3664546399000423895061894810877788924077

;; soc_main_code: 37 instructions followed by its two data words.
verifier_start:
    ;; soc_main_code_init0
    mov r1 pc
    lea r1 (verifier_callback-verifier_start)
    restrict r1 E
    jmp r0

verifier_callback:
    ;; soc_main_code_callback0
    mov r3 pc
    mov r4 r3
    lea r3 (verifier_fail-verifier_callback)

    getotype r2 r0
    sub r2 r2 -1
    mov r5 pc
    lea r5 4
    jnz r5 r2
    jmp r3

    getotype r2 r0
    estoreid r4 r2
    sub r4 r4 SOC_HASH
    jnz r3 r4

    unseal r1 r1 r0
    mov r0 r5
    geta r4 r1
    mov r5 42
    mov r1 r3
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

;; A concrete 14-instruction witness for the arbitrary adversary in
;; soc_adequacy.v.  It destroys its broad initial authority before EInit.
adversary_start:
    mov r3 pc
    subseg pc adversary_start adversary_end
    subseg r3 soc_enclave_start soc_enclave_end
    restrict r3 RX
    subseg r0 soc_enclave_data soc_enclave_data_end
    restrict r0 RW
    einit r3 r0

adversary_call:
    mov r0 pc
    lea r0 (adversary_return-adversary_call)
    restrict r0 E
    mov r31 r1
    jmp r3
adversary_return:
    mov r0 r2
    jmp r31
adversary_end:

;; soc_enclave_instrs.  EInit reserves the first cell for the enclave's data
;; capability and hashes the 20 integer-encoded instructions that follow it.
soc_enclave_start:
    #0
soc_enclave_entry:
    mov r1 pc
    lea r1 -1
    load r1 r1
    getb r2 r1
    geta r3 r1
    sub r2 r2 r3
    lea r1 r2
    load r1 r1
    gete r3 r1
    sub r2 r3 1
    subseg r1 r2 r3

    mov r2 pc
    geta r3 r2
    sub r3 42 r3
    lea r2 r3
    restrict r2 O
    lea r1 1
    seal r2 r1 r2

    restrict r1 U
    jmp r0
soc_enclave_end:

soc_enclave_data:
    #0
    #0
soc_enclave_data_end:

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
