%define SWITCHER_CALL_OFFSET 0
%define ASSERT_OFFSET 1
%define ADV_F_OFFSET 2
%define KVS_INSERT_OFFSET 3
%define KVS_READ_OFFSET 4
%define KVS_ERASE_OFFSET 5
%define SEALED_USER_KEY_OFFSET 6
%define ADV_KVS_INSERT_OFFSET 1
%define ADV_KVS_ERASE_OFFSET 3
%define ADV_SEALED_USER_KEY_OFFSET 4
%define SIZE_MAP 16
%define ASM_SIZEOF_KVS_ENTRY 3
%define UNSEALING_USER_KEY_OFFSET 1
%define UINT16_MIN 0
%define UINT16_MAX 65536
%define ASM_TRUE 0
%define ASM_FALSE -1
%define ASM_NONE 0
%define ASM_SOME 1
%define EMPTY_SLOT -1
%define DEFAULT_VAL 0

%macro fetch(offset: expr, dst: reg, scratch1: reg, scratch2: reg)
    mov $dst PC
    getb $scratch1 $dst
    geta $scratch2 $dst
    sub $scratch1 $scratch1 $scratch2
    lea $dst $scratch1
    lea $dst $offset
    load $dst $dst
    mov $scratch1 0
    mov $scratch2 0
%endmacro

%macro assert_eq(offset: expr, dst: reg, scratch1: reg, scratch2: reg)
    mov $dst PC
    getb $scratch1 $dst
    geta $scratch2 $dst
    sub $scratch1 $scratch1 $scratch2
    lea $dst $scratch1
    lea $dst $offset
    load $dst $dst
    mov $scratch1 0
    mov $scratch2 0
    mov $scratch1 cra
    jalr cra $dst
    mov cra $scratch1
    mov $scratch1 0
    mov $dst 0
%endmacro

%macro kvs_check_uint16(value: reg, dst: reg)
    lt $dst (UINT16_MIN - 1) $value
    jnz $dst (uint16_min - &CURRENT_ADDR)
    mov $dst ASM_FALSE
    jmp (uint16_end - &CURRENT_ADDR)
uint16_min:
    lt $dst $value UINT16_MAX
    jnz $dst (uint16_max - &CURRENT_ADDR)
    mov $dst ASM_FALSE
    jmp (uint16_end - &CURRENT_ADDR)
uint16_max:
    mov $dst ASM_TRUE
uint16_end:
%endmacro

%macro kvs_get_full_key(dst: reg, sealed_key: reg, map_key: reg, scratch1: reg, scratch2: reg)
    mov $dst PC
    getb $scratch1 $dst
    geta $scratch2 $dst
    sub $scratch1 $scratch1 $scratch2
    lea $dst $scratch1
    lea $dst UNSEALING_USER_KEY_OFFSET
    load $dst $dst
    unseal $dst $dst $sealed_key
    load $dst $dst
    lshiftl $dst $dst 16
    lor $dst $dst $map_key
%endmacro

%macro kvs_search(key: reg, index: reg, empty_index: reg, scratch: reg)
    mov $index 0
    mov $empty_index EMPTY_SLOT
search_loop:
    sub $scratch SIZE_MAP $index
    jnz $scratch (search_body - &CURRENT_ADDR)
    jmp (search_not_found - &CURRENT_ADDR)
search_body:
    load $scratch cgp
    jnz $scratch (search_some - &CURRENT_ADDR)
    mov $empty_index $index
    lea cgp ASM_SIZEOF_KVS_ENTRY
    add $index $index 1
    jmp (search_loop - &CURRENT_ADDR)
search_some:
    lea cgp 1
    load $scratch cgp
    sub $scratch $key $scratch
    jnz $scratch (search_different_key - &CURRENT_ADDR)
    lea cgp -1
    jmp (search_end - &CURRENT_ADDR)
search_different_key:
    lea cgp 2
    add $index $index 1
    jmp (search_loop - &CURRENT_ADDR)
search_not_found:
    lea cgp (-(ASM_SIZEOF_KVS_ENTRY * SIZE_MAP))
    mov $index EMPTY_SLOT
search_end:
%endmacro

    ;; main is just a very basic loader that jumps to the main compartment A
    ;; and terminates the machine
loader:
    #([X Ow LG LM], Global, C, C_end, C_main)           ; PCC_C + main
    #([R W LG LM], Global, C_data, C_data_end, C_data)  ; CGP_C

loader_main:
    mov cra PC
    lea cra -1
    load cgp cra                 ; puts CGP_C in CGP
    lea cra -1
    load cra cra
    jalr cra cra                  ; jumps to C_main
    fail                        ; should never been reach, as C_main terminates the program
loader_end:

;; Client
C:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc) ; import switcher
    #(E-[X Ow LG LM], Global, assert, assert_end, assert)                 ; assert
    #{9: ([R Ow LG LM], Global, A_ext, A_ext_end, A_ext_adv)} ; import A.adv
    #{9: ([R Ow LG LM], Global, KVS_ext, KVS_ext_end, KVS_ext_insert)} ; import KVS.insert
    #{9: ([R Ow LG LM], Global, KVS_ext, KVS_ext_end, KVS_ext_read)} ; import KVS.read
    #{9: ([R Ow LG LM], Global, KVS_ext, KVS_ext_end, KVS_ext_erase)} ; import KVS.erase
    #{10: ([R Ow LG LM], Global, C_ssealing, C_ssealing_end, C_ssealing)} ; kvs_user_seal_key
C_main:
    %fetch(SEALED_USER_KEY_OFFSET, cs1, ct0, ct1)
    ;; addOrUpdate(sealedUserKey, 1, 12)
    mov ca0 cs1
    mov ca1 1
    mov ca2 12

    %fetch(SWITCHER_CALL_OFFSET, ctp, ct0, ct1)
    %fetch(KVS_INSERT_OFFSET, ct1, ct0, cs0)
    ;; jump
    jalr cra ctp

    ;; (* check if inserted *)
    jnz ca0 2
    ;; (* case INSERT_PASS *)
    jmp 2
    ;; (* case INSERT_FAIL / ENOTENOUGHTRUSTEDSTACK *)
    halt
    ;; (* case INSERT_PASS *)
    mov ca0 0
    mov ca1 0

    ;; adv.f()
    %fetch(SWITCHER_CALL_OFFSET, ctp, ct0, ct1)
    %fetch(ADV_F_OFFSET, ct1, ct0, cs0)

    ;; jump
    jalr cra ctp
    ;; res = read(sealedUserKey, 1)
      ;; (* read(sealedUserKey, 1)*)
    mov ca0 cs1
    mov ca1 1

    %fetch(SWITCHER_CALL_OFFSET, ctp, ct0, ct1)
    %fetch(KVS_READ_OFFSET, ct1, ct0, cs0)
    ;; jump
    jalr cra ctp

    ;; (* assert (ret == 12) *)
    ;; (* check if ENOTENOUGHTRUSTEDSTACK *)
    jnz ca0 2
    ;; (* case PASS *)
    jmp 2
    ;; (* case ENOTENOUGHTRUSTEDSTACK *)
    halt
    ;; (* case PASS *)
    mov ct0 ca1
    mov ct1 12
    %assert_eq(ASSERT_OFFSET, ct2, ct3, ct4)
    halt
C_main_end:
C_end:

C_ssealing:
    #0x01
C_ssealing_end:

C_data:
C_data_end:

;; export table compartment C -> does not export any entry points
C_ext:
    #([X Ow LG LM], Global, C, C_end, C)                 ; PCC
    #([R W LG LM], Global, C_data, C_data_end, C_data)   ; CGP
C_ext_end:

;; Adversary
A:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc) ; import switcher
    #{9: ([R Ow LG LM], Global, KVS_ext, KVS_ext_end, KVS_ext_insert)} ; import KVS.insert
    #{9: ([R Ow LG LM], Global, KVS_ext, KVS_ext_end, KVS_ext_read)} ; import KVS.read
    #{9: ([R Ow LG LM], Global, KVS_ext, KVS_ext_end, KVS_ext_erase)} ; import KVS.erase
    #{10: ([R Ow LG LM], Global, A_ssealing, A_ssealing_end, A_ssealing)} ; kvs_user_seal_key
    ;; no import
A_adv:
    ;; Insert and then erase key 1 in the adversary's own KVS namespace.
    %fetch(ADV_SEALED_USER_KEY_OFFSET, cs1, ct0, ct1)
    mov cs0 cra
    mov ca0 cs1
    mov ca1 1
    mov ca2 42
    %fetch(SWITCHER_CALL_OFFSET, ctp, ct0, ct1)
    %fetch(ADV_KVS_INSERT_OFFSET, ct1, ct0, ct2)
    jalr cra ctp

    mov ca0 cs1
    mov ca1 1
    %fetch(SWITCHER_CALL_OFFSET, ctp, ct0, ct1)
    %fetch(ADV_KVS_ERASE_OFFSET, ct1, ct0, ct2)
    jalr cra ctp

    mov cra cs0
    mov ca0 0
    mov ca1 0
    mov ca2 0
    jalr cnull cra
A_end:

A_ssealing:
    #0x02
A_ssealing_end:

A_data:
    #0x0
A_data_end:

;; export table compartment A -> exports A_f
A_ext:
    #([X Ow LG LM], Global, A, A_end, A)                 ; PCC
    #([R W LG LM], Global, A_data, A_data_end, A_data)   ; CGP
A_ext_adv: #(((A_adv - A) << 3) || 0)                    ; offset_f
A_ext_end:


;; KVS
KVS:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc) ; import switcher
    #[SU, Global, 10, 11, 10] ;; unsealing key for KVS
KVS_insert:
    %kvs_check_uint16(ca1, ct1)

    jnz ct1  (KVS_insert_not_uint16-&CURRENT_ADDR)
KVS_insert_uint16:
    jmp (KVS_insert_uint16_check_pass-&CURRENT_ADDR)
KVS_insert_not_uint16:
    mov ca0 ASM_FALSE
    mov ca1 0
    jalr cnull cra
KVS_insert_uint16_check_pass:

    %kvs_get_full_key(ctp, ca0, ca1, ct1, ct2)

    mov ca0 ctp
    %kvs_search(ca0, ctp, ct1, ct2)

    sub ctp ctp (-1)
    jnz ctp (KVS_insert_key_found-&CURRENT_ADDR)

KVS_insert_key_not_found:
    sub ctp ct1 (-1)
    jnz ctp (KVS_insert_empty_slot_found-&CURRENT_ADDR)

KVS_insert_empty_slot_not_found:
    mov ca0 ASM_FALSE
    mov ca1 0
    jalr cnull cra

KVS_insert_empty_slot_found:
    mul ct1 ct1 ASM_SIZEOF_KVS_ENTRY
    lea cgp ct1
    store cgp ASM_SOME
    lea cgp 1
    store cgp ca0
    lea cgp 1
    store cgp ca2
    mov ca0 ASM_TRUE
    mov ca1 0
    jalr cnull cra

KVS_insert_key_found:
    lea cgp 2
    store cgp ca2
    mov ca0 ASM_TRUE
    mov ca1 0
    jalr cnull cra
KVS_insert_end:

KVS_read:
    %kvs_check_uint16(ca1, ct1)

    jnz ct1 (KVS_read_not_uint16-&CURRENT_ADDR)
KVS_read_uint16:
    jmp (KVS_read_uint16_check_pass-&CURRENT_ADDR)
KVS_read_not_uint16:
    mov ca0 ASM_FALSE
    mov ca1 0
    jalr cnull cra
KVS_read_uint16_check_pass:

    %kvs_get_full_key(ctp, ca0, ca1, ct1, ct2)

    mov ca0 ctp
    %kvs_search(ca0, ctp, ct1, ct2)

    sub ctp ctp (-1)
    jnz ctp (KVS_read_key_found-&CURRENT_ADDR)
KVS_read_key_not_found:
    mov ca0 ASM_FALSE
    mov ca1 0
    jmp (KVS_read_key_ret-&CURRENT_ADDR)
KVS_read_key_found:
    lea cgp 2
    load ca1 cgp
    mov ca0 ASM_TRUE
KVS_read_key_ret:
    jalr cnull cra
KVS_read_end:

KVS_erase:
    %kvs_check_uint16(ca1, ct1)
    jnz ct1 (KVS_erase_not_uint16-&CURRENT_ADDR)
KVS_erase_uint16:
    jmp (KVS_erase_uint16_check_pass-&CURRENT_ADDR)
KVS_erase_not_uint16:
    mov ca0 ASM_FALSE
    mov ca1 0
    jalr cnull cra
KVS_erase_uint16_check_pass:

    %kvs_get_full_key(ctp, ca0, ca1, ct1, ct2)
    mov ca0 ctp
    %kvs_search(ca0, ctp, ct1, ct2)
    sub ctp ctp (-1)
    jnz ctp (KVS_erase_key_found-&CURRENT_ADDR)
KVS_erase_key_not_found:
    jmp (KVS_erase_return-&CURRENT_ADDR)
KVS_erase_key_found:
    store cgp ASM_NONE
KVS_erase_return:
    mov ca0 0
    mov ca1 0
    jalr cnull cra
KVS_erase_end:
KVS_end:


KVS_data:
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
    #ASM_NONE
    #EMPTY_SLOT
    #DEFAULT_VAL
KVS_data_end:

;; export table compartment C -> exports C_g
KVS_ext:
    #([X Ow LG LM], Global, KVS, KVS_end, KVS)                 ; PCC
    #([R W LG LM], Global, KVS_data, KVS_data_end, KVS_data)   ; CGP
KVS_ext_insert: #(((KVS_insert - KVS) << 3) || 3)                         ; offset_insert
KVS_ext_read: #(((KVS_read - KVS) << 3) || 2)                         ; offset_read
KVS_ext_erase: #(((KVS_erase - KVS) << 3) || 2)                         ; offset_erase
KVS_ext_end:


;; Assert library from theories/case_studies/macros/assert.v.
assert:
    sub ct0 ct0 ct1
    jnz ct0 (assert_fail - &CURRENT_ADDR)
assert_success:
    mov ct0 0
    mov ct1 0
    jalr cnull cra
assert_fail:
    mov ct1 PC
    ;; ct1 contains the address of the preceding mov, hence the +1.
    lea ct1 (assert_flag_cap + 1 - &CURRENT_ADDR)
    load ct1 ct1
    store ct1 1
    mov ct0 0
    mov ct1 0
    jalr cnull cra
assert_flag_cap:
    #([R W LG LM], Global, assert_flag, assert_data_end, assert_flag)
assert_end:
assert_flag:
    #0
assert_data_end:

;; Concatenate this file at the end of any example that require the switcher
%define ECOMPARTMENTFAIL -1
%define ENOTENOUGHTRUSTEDSTACK -141

switcher:
    #[SU, Global, 9, 10, 9]
switcher_cc:
    getp ct2 csp
    mov ctp [R WL LG LM]
    sub ct2 ct2 ctp
    jnz ct2 (switcher_force_unwind - &CURRENT_ADDR)
    getl ct2 csp
    mov ctp Local
    sub ct2 ct2 ctp
    jnz ct2 (switcher_force_unwind - &CURRENT_ADDR)
    store csp cs0
    lea csp 1
    store csp cs1
    lea csp 1
    store csp cra
    lea csp 1
    store csp cgp
    lea csp 1
    readsr ct2 mtdc
    geta cs0 ct2
    add cs0 cs0 1
    gete ctp ct2
    lt ctp cs0 ctp
    jnz ctp 2
    jmp (switcher_trusted_stack_exhausted - &CURRENT_ADDR)
    lea ct2 1
    store ct2 csp
    writesr mtdc ct2
    gete cs0 csp
    geta cs1 csp
    subseg csp cs1 cs0
switcher_zero_stk_init_pre:
    sub cs0 cs1 cs0
    mov cs1 csp
switcher_zero_stk_loop_pre:
    jnz cs0 2
    jmp (switcher_zero_stk_end_pre - switcher_zero_stk_loop_pre - 1)
    store cs1 0
    lea cs1 1
    add cs0 cs0 1
switcher_zero_stk_loop_end_pre:
    jmp (switcher_zero_stk_loop_pre - switcher_zero_stk_loop_end_pre)
switcher_zero_stk_end_pre:
    getb cs1 PC
    geta cs0 PC
    sub cs1 cs1 cs0
    mov cs0 PC
    lea cs0 cs1
    lea cs0 -2
    load cs0 cs0
    unseal ct1 cs0 ct1
    load cs0 ct1
    land ct2 cs0 7
    lshiftr cs0 cs0 3
    getb cgp ct1
    geta cs1 ct1
    sub cs1 cgp cs1
    lea ct1 cs1
    load cra ct1
    lea ct1 1
    load cgp ct1
    lea cra cs0
    add ct2 ct2 1
    jmp ct2
    mov r10 0
    mov r11 0
    mov r12 0
    mov r13 0
    mov r14 0
    mov r15 0
    mov r5 0
    mov r0 0
    mov r4 0
    mov r6 0
    mov r7 0
    mov r8 0
    mov r9 0
    mov r16 0
    mov r17 0
    mov r18 0
    mov r19 0
    mov r20 0
    mov r21 0
    mov r22 0
    mov r23 0
    mov r24 0
    mov r25 0
    mov r26 0
    mov r27 0
    mov r28 0
    mov r29 0
    mov r30 0
    jalr cra cra
switcher_after_compartment_call:
    readsr ctp mtdc
    load csp ctp
    lea ctp -1
    writesr mtdc ctp
    lea csp -1
    load cgp csp
    lea csp -1
    load cra csp
    lea csp -1
    load cs1 csp
    lea csp -1
    load cs0 csp
switcher_zero_stk_init_post:
    gete ct0 csp
    geta ct1 csp
    sub ct0 ct1 ct0
    mov ct1 csp
switcher_zero_stk_loop_post:
    jnz ct0 2
    jmp (switcher_zero_stk_end_post - switcher_zero_stk_loop_post - 1)  ;
    store ct1 0
    lea ct1 1
    add ct0 ct0 1
switcher_zero_stk_loop_end_post:
    jmp (switcher_zero_stk_loop_post - switcher_zero_stk_loop_end_post)
switcher_zero_stk_end_post:
switcher_callee_dead_zeros:
    mov r0 0
    mov r4 0
    mov r5 0
    mov r6 0
    mov r7 0
    mov r12 0
    mov r13 0
    mov r14 0
    mov r15 0
    mov r16 0
    mov r17 0
    mov r18 0
    mov r19 0
    mov r20 0
    mov r21 0
    mov r22 0
    mov r23 0
    mov r24 0
    mov r25 0
    mov r26 0
    mov r27 0
    mov r28 0
    mov r29 0
    mov r30 0
    jalr cnull cra
switcher_trusted_stack_exhausted:
    lea csp -1
    load cgp csp
    lea csp -1
    load cra csp
    lea csp -1
    load cs1 csp
    lea csp -1
    load cs0 csp
    mov ca0 ENOTENOUGHTRUSTEDSTACK
    mov ca1 0
    jmp (switcher_callee_dead_zeros - &CURRENT_ADDR)
switcher_force_unwind:
    mov ca0 ECOMPARTMENTFAIL
    mov ca1 0
    jmp (switcher_after_compartment_call - &CURRENT_ADDR)
switcher_end:
