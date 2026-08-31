;; Mutual attestation, faithfully instantiated from the pinned Rocq artifact at
;; cerisier 57ed584ae17eed308ae0fa554cf0dde9843112c1.
;;
;; The verifier and both enclaves are literal translations of
;; mutual_attestation_code.v.  The only witness-specific code is the arbitrary
;; 36-instruction adversary below.  Rocq's r_tN aliases map directly to rN, Mod
;; is written rem, and hash_cap_instrs/assert_reg_instrs are expanded in place.
;; The four constants are derived from EInit in staged fresh executions.

%define MA_PRE_A 28249013258660799722221594376824205332309
%define MA_PRE_B 23075161495184733793446375969728546363081
%define MA_HASH_A 28586538272572871769842225592882347710580
%define MA_HASH_B 23463451373229852078782113054873498149463

;; mutual_attestation_main_code 0: 72 instructions, then its link capability.
verifier_start:
    mov r1 pc
    lea r1 4
    restrict r1 E
    jmp r0

verifier_callback:
    ;; mutual_attestation_main_attest_or_fail r0 MA_HASH_A
    getotype r5 r0
    estoreid r6 r5
    sub r6 r6 MA_HASH_A
    mov r5 pc
    lea r5 5
    jnz r5 r6
    lea r5 2
    jmp r5
    fail
    mov r5 r5

    ;; mutual_attestation_main_get_confirm_or_fail r0 r1
    unseal r0 r1 r0
    getb r5 r0
    rem r5 r5 2
    mov r6 pc
    lea r6 4
    jnz r6 r5
    fail
    geta r0 r0

    ;; mutual_attestation_main_attest_or_fail r2 MA_HASH_B
    getotype r5 r2
    estoreid r6 r5
    sub r6 r6 MA_HASH_B
    mov r5 pc
    lea r5 5
    jnz r5 r6
    lea r5 2
    jmp r5
    fail
    mov r5 r5

    ;; mutual_attestation_main_get_confirm_or_fail r2 r3
    unseal r2 r3 r2
    getb r5 r2
    rem r5 r5 2
    mov r6 pc
    lea r6 4
    jnz r6 r5
    fail
    geta r2 r2

    mov r6 r2
    mov r1 pc
    gete r4 r1
    geta r5 r1
    sub r4 r4 r5
    lea r1 r4
    lea r1 -1
    load r1 r1
    mov r3 r1

    ;; assert_reg_instrs 0 r1: assert r0 == 1.
    mov r4 r0
    mov r5 1
    lea r1 0
    load r1 r1
    mov r2 r0
    mov r0 pc
    lea r0 3
    jmp r1
    mov r0 r2
    mov r1 0
    mov r2 0

    ;; assert_reg_instrs 0 r3: assert the saved B result == 1.
    mov r4 r6
    mov r5 1
    lea r3 0
    load r3 r3
    mov r2 r0
    mov r0 pc
    lea r0 3
    jmp r3
    mov r0 r2
    mov r3 0
    mov r2 0
    halt

verifier_link_cap:
    #(RO, assert_link, assert_link_end, assert_link)
verifier_end:

;; Concrete witness for the arbitrary adequacy adversary.  Its initial region
;; contains this code, both enclaves, both data regions, and the parity pads.
adversary_start:
    mov r10 pc
    mov r11 r10
    mov r12 r10
    subseg pc adversary_start adversary_end
    subseg r10 enclave_a_start enclave_a_end
    restrict r10 RX
    subseg r11 enclave_a_data enclave_a_data_end
    restrict r11 RW
    subseg r12 enclave_b_start enclave_b_end
    restrict r12 RX
    subseg r0 enclave_b_data enclave_b_data_end
    restrict r0 RW
    einit r10 r11
    einit r12 r0

    mov r31 r1                 ;; verifier callback
call_a_first:
    mov r0 pc
    lea r0 (after_a_first-call_a_first)
    restrict r0 E
    jmp r10
after_a_first:
    mov r20 r3                 ;; A's private resume sentry

call_b:
    mov r0 pc
    lea r0 (after_b-call_b)
    restrict r0 E
    jmp r12
after_b:
    mov r23 r2                 ;; {1}_B
    mov r24 r3                 ;; public unseal key B

call_a_resume:
    mov r0 pc
    lea r0 (after_a_resume-call_a_resume)
    restrict r0 E
    mov r2 r24
    jmp r20
after_a_resume:
    mov r0 r1                  ;; {1}_A
    mov r1 r2                  ;; public unseal key A
    mov r2 r23                 ;; {1}_B
    mov r3 r24                 ;; public unseal key B
    jmp r31
adversary_end:

;; --------------------------------------------------------------------------
;; Enclave A
;; --------------------------------------------------------------------------

enclave_a_start:
    #0                          ;; overwritten by EInit with A's data cap
enclave_a_entry:
    ;; Fetch data cap from the first enclave word.
    mov r5 pc
    geta r1 r5
    getb r2 r5
    sub r1 r2 r1
    lea r5 r1
    load r1 r5

    ;; Fetch A's EInit-generated sealing range.
    geta r2 r1
    getb r3 r1
    sub r2 r3 r2
    lea r1 r2
    load r6 r1

    ;; Expanded mutual_attest_enclave_A_mod_encoding_42_instrs.
    getb r2 r1
    add r3 r2 1
    rem r4 r2 2
    mov r5 pc
    lea r5 6
    jnz r5 r4
    subseg r1 r2 r3
    lea r5 2
    jmp r5
    add r4 r3 1
    subseg r1 r3 r4
    sub r3 42 r2
    lea r1 r3
    restrict r1 O

    ;; Sign {42} with the private signing half, publish only the unseal half.
    lea r6 1
    seal r1 r6 r1
    geta r3 r6
    add r4 r3 1
    subseg r6 r3 r4
    restrict r6 U

    ;; Return to bootstrap, retaining a sentry for the second phase in r3.
    mov r3 pc
    lea r3 8
    restrict r3 E
    mov r2 r6
    mov r4 0
    mov r5 0
    mov r6 0
    jmp r0

enclave_a_resume:
    ;; Reconstruct B's full identity from the embedded pre-hash table.
    mov r4 pc
    geta r5 r4
    gete r6 r4
    sub r5 r6 r5
    lea r4 r5
    lea r4 -2
    mov r3 r4
    lea r3 1
    load r3 r3
    geta r5 r4
    subseg r4 r5 r6
    mov r11 r1
    mov r12 r2
    mov r13 r3
    mov r15 r5
    mov r16 r6

    ;; Exact 29-instruction hash_cap_instrs; result is returned in r8.
    getb r1 r4
    geta r2 r4
    sub r2 r1 r2
    lea r4 r2
    gete r5 r4
    sub r5 r5 1
    load r7 r4
    hash r8 r7
    lea r4 1
    add r1 r1 1
    mov r2 pc
    lea r2 12
    mov r3 pc
    lea r3 2
    lt r6 r5 r1
    jnz r2 r6
    load r7 r4
    hash r7 r7
    hashconcat r8 r8 r7
    lea r4 1
    add r1 r1 1
    jmp r3
    mov r1 0
    mov r2 0
    mov r3 0
    mov r4 0
    mov r5 0
    mov r6 0
    mov r7 0

    mov r1 r11
    mov r2 r12
    mov r3 r13
    mov r4 r8
    mov r5 r15
    mov r6 r16
    mov r7 0
    mov r8 0
    mov r11 0
    mov r12 0
    mov r13 0
    mov r15 0
    mov r16 0
    hashconcat r3 r3 r4

    ;; Check that B supplied a sealed word.
    getotype r4 r1
    add r4 r4 1
    mov r5 pc
    lea r5 4
    jnz r5 r4
    fail

    ;; Compare B's signed object type with the reconstructed identity.
    getotype r5 r1
    estoreid r4 r5
    sub r3 r3 r4
    mov r5 pc
    lea r5 5
    jnz r5 r3
    lea r5 1
    jmp r5
    fail

    ;; Unseal B's response and assert an even payload base and value 43.
    unseal r1 r2 r1
    getb r2 r1
    rem r2 r2 2
    mov r5 pc
    lea r5 5
    jnz r5 r2
    lea r5 1
    jmp r5
    fail
    geta r1 r1
    sub r1 r1 43
    lea r5 6
    jnz r5 r2
    lea r5 1
    jmp r5
    fail

    ;; Fetch A's data cap and sealing range again.
    geta r1 r5
    getb r2 r5
    sub r1 r2 r1
    lea r5 r1
    load r1 r5
    geta r2 r1
    getb r3 r1
    sub r2 r3 r2
    lea r1 r2
    load r6 r1

    ;; Expanded mutual_attest_enclave_A_mod_encoding_1_instrs.
    getb r2 r1
    add r3 r2 1
    rem r4 r2 2
    mov r5 pc
    lea r5 7
    jnz r5 r4
    add r4 r3 1
    subseg r1 r3 r4
    lea r5 1
    jmp r5
    subseg r1 r2 r3
    sub r3 1 r2
    lea r1 r3
    restrict r1 O

    ;; Sign A's confirmation and return it with the public unseal half.
    lea r6 1
    seal r1 r6 r1
    geta r3 r6
    add r4 r3 1
    subseg r6 r3 r4
    restrict r6 U
    mov r2 r6
    mov r3 0
    mov r4 0
    mov r5 0
    mov r6 0
    jmp r0

enclave_a_identity_table:
    #MA_PRE_A
    #MA_PRE_B
enclave_a_end:

    ;; A's code ends at an odd address; pad its two-cell data block to even.
    #0
enclave_a_data:
    #0
    #0
enclave_a_data_end:

;; --------------------------------------------------------------------------
;; Enclave B
;; --------------------------------------------------------------------------

enclave_b_start:
    #0                          ;; overwritten by EInit with B's data cap
enclave_b_entry:
    ;; Reconstruct A's full identity from the embedded pre-hash table.
    mov r4 pc
    geta r5 r4
    gete r6 r4
    sub r5 r6 r5
    lea r4 r5
    lea r4 -2
    mov r3 r4
    lea r3 0
    load r3 r3
    geta r5 r4
    subseg r4 r5 r6
    mov r11 r1
    mov r12 r2
    mov r13 r3
    mov r15 r5
    mov r16 r6

    ;; Exact 29-instruction hash_cap_instrs; result is returned in r8.
    getb r1 r4
    geta r2 r4
    sub r2 r1 r2
    lea r4 r2
    gete r5 r4
    sub r5 r5 1
    load r7 r4
    hash r8 r7
    lea r4 1
    add r1 r1 1
    mov r2 pc
    lea r2 12
    mov r3 pc
    lea r3 2
    lt r6 r5 r1
    jnz r2 r6
    load r7 r4
    hash r7 r7
    hashconcat r8 r8 r7
    lea r4 1
    add r1 r1 1
    jmp r3
    mov r1 0
    mov r2 0
    mov r3 0
    mov r4 0
    mov r5 0
    mov r6 0
    mov r7 0

    mov r1 r11
    mov r2 r12
    mov r3 r13
    mov r4 r8
    mov r5 r15
    mov r6 r16
    mov r7 0
    mov r8 0
    mov r11 0
    mov r12 0
    mov r13 0
    mov r15 0
    mov r16 0
    hashconcat r3 r3 r4

    ;; Check that A supplied a sealed word.
    getotype r4 r1
    add r4 r4 1
    mov r5 pc
    lea r5 4
    jnz r5 r4
    fail

    ;; Compare A's signed object type with the reconstructed identity.
    getotype r5 r1
    estoreid r4 r5
    sub r3 r3 r4
    mov r5 pc
    lea r5 5
    jnz r5 r3
    lea r5 1
    jmp r5
    fail

    ;; Unseal A's request and assert an even payload base and value 42.
    unseal r1 r2 r1
    getb r2 r1
    rem r2 r2 2
    mov r5 pc
    lea r5 5
    jnz r5 r2
    lea r5 1
    jmp r5
    fail
    geta r1 r1
    sub r1 r1 42
    lea r5 6
    jnz r5 r2
    lea r5 1
    jmp r5
    fail

    ;; Fetch B's data cap and EInit-generated sealing range.
    geta r1 r5
    getb r2 r5
    sub r1 r2 r1
    lea r5 r1
    load r1 r5
    geta r2 r1
    getb r3 r1
    sub r2 r3 r2
    lea r1 r2
    load r6 r1

    ;; Split the two data cells before positioning their protected cursors.
    mov r4 r1
    getb r2 r1
    add r3 r2 1
    subseg r1 r2 r3
    add r5 r3 1
    subseg r4 r3 r5

    ;; Expanded mutual_attest_enclave_B_mod_encoding_instrs.
    rem r3 r2 2
    mov r5 pc
    lea r5 9
    jnz r5 r3
    sub r3 43 r2
    lea r1 r3
    sub r3 1 r2
    lea r4 r3
    lea r5 4
    jmp r5
    sub r3 1 r2
    lea r1 r3
    sub r3 43 r2
    lea r4 r3

    restrict r1 O
    restrict r4 O

    ;; Sign {43} and {1}; return both plus B's public unseal half.
    lea r6 1
    seal r1 r6 r1
    seal r2 r6 r4
    geta r3 r6
    add r4 r3 1
    subseg r6 r3 r4
    restrict r6 U
    mov r3 r6
    mov r4 0
    mov r5 0
    mov r6 0
    jmp r0

enclave_b_identity_table:
    #MA_PRE_A
    #MA_PRE_B
enclave_b_end:

    ;; Keep B's two-cell block even-aligned, so B returns {43} in r1 and {1}
    ;; in r2.  EInit overwrites each data block's first word with a seal range.
    #0
enclave_b_data:
    #0
    #0
enclave_b_data_end:

;; Exact 13-instruction assert_subroutine_instrs, followed by its capability
;; and flag.  A mismatch records 1 and returns to the verifier.
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
