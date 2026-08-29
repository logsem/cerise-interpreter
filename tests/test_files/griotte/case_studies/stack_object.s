;; Stack objects: validate an incoming stack object before sharing it together
;; with a fresh object, while keeping the callee's secret stack slot private.
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

%macro checkra(src: reg, scratch1: reg, scratch2: reg)
    getwtype $scratch1 $src
    sub $scratch2 $scratch1 Cap
    jnz $scratch2 4
    getp $scratch1 $src
    sub $scratch2 $scratch1 [Orx Ow LG LM]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx Ow DL LM]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx Ow LG DRO]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx Ow DL DRO]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx W LG LM]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx W DL LM]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx W LG DRO]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx W DL DRO]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx WL LG LM]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx WL DL LM]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx WL LG DRO]
    jnz $scratch2 2
    fail
    sub $scratch2 $scratch1 [Orx WL DL DRO]
    jnz $scratch2 2
    fail
    mov $scratch1 0
    mov $scratch2 0
%endmacro

%macro check_no_overlap(src1: reg, src2: reg, scratch1: reg, scratch2: reg)
    getb $scratch1 $src1
    getb $scratch2 $src2
    lt $scratch1 $scratch1 $scratch2
    jnz $scratch1 (first_below_second - &CURRENT_ADDR)
    getb $scratch1 $src1
    gete $scratch2 $src2
    sub $scratch2 $scratch2 1
    lt $scratch1 $scratch2 $scratch1
    jnz $scratch1 (second_end_below_first - &CURRENT_ADDR)
    fail
second_end_below_first:
    jmp (no_overlap_end - &CURRENT_ADDR)
first_below_second:
    getb $scratch2 $src2
    gete $scratch1 $src1
    sub $scratch1 $scratch1 1
    lt $scratch1 $scratch1 $scratch2
    jnz $scratch1 (first_end_below_second - &CURRENT_ADDR)
    fail
first_end_below_second:
    jmp (no_overlap_end - &CURRENT_ADDR)
no_overlap_end:
    mov $scratch1 0
    mov $scratch2 0
%endmacro

%macro checkints(src: reg, scratch1: reg, scratch2: reg)
    getb $scratch1 $src
    geta $scratch2 $src
    sub $scratch1 $scratch1 $scratch2
    lea $src $scratch1
    mov $scratch1 0
    mov $scratch2 0
    getb $scratch1 $src
    gete $scratch2 $src
    lt $scratch1 $scratch1 $scratch2
    sub $scratch1 $scratch1 1
    jnz $scratch1 12
    load $scratch1 $src
    getwtype $scratch2 $scratch1
    sub $scratch2 $scratch2 Int
    jnz $scratch2 2
    jmp 2
    fail
    lea $src 1
    geta $scratch1 $src
    gete $scratch2 $src
    lt $scratch1 $scratch1 $scratch2
    jnz $scratch1 -10
    mov $scratch1 0
    mov $scratch2 0
%endmacro

loader:
    #([X Ow LG LM], Global, StackObject, StackObject_end, StackObject_run)
    #([R W LG LM], Global, StackObject_data, StackObject_data_end, StackObject_data)
loader_main:
    mov cra PC
    lea cra -1
    load cgp cra
    lea cra -1
    load cra cra
    jalr cra cra
    fail
loader_end:

StackObject:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
    #(E-[X Ow LG LM], Global, assert, assert_end, assert)
    #{9: ([R Ow LG LM], Global, B_ext, B_ext_end, B_ext_adv)}
StackObject_run:
    %fetch(0, ct0, cs0, cs1)
    %fetch(2, ct1, cs0, cs1)
    jalr cra ct0
    halt

StackObject_f:
    ;; ca0 is the caller's stack object; ca1 is its callback g.
    mov ct1 ca1
    %checkra(ca0, cs0, cs1)
    %check_no_overlap(ca0, csp, cs0, cs1)
    %checkints(ca0, cs0, cs1)
    ;; Allocate the hidden secret followed by a one-word public object.
    store csp 42
    lea csp 1
    mov ca1 csp
    geta cs0 ca1
    add cs1 cs0 1
    subseg ca1 cs0 cs1
    store ca1 0
    lea csp 1
    %fetch(0, ct0, cs0, cs1)
    mov cs0 cra
    mov cs1 ct1
    jalr cra ct0
    lea csp -2
    load ct0 csp
    mov ct1 42
    %assert_eq(1, ct2, ct3, ct4)
    mov cra cs0
    mov ca0 0
    mov ca1 0
    jalr cnull cra
StackObject_end:

B:
    #(E-[XSR Ow LG LM], Local, switcher, switcher_end, switcher_cc)
    #{9: ([R Ow LG LM], Global, SO_ext, SO_ext_end, SO_ext_f)}
    #{9: ([R Ow LG LM], Global, B_ext, B_ext_end, B_ext_g)}
B_adv:
    ;; Allocate an integer-only object on B's frame and call SO.f(in, B.g).
    store csp 7
    mov ca0 csp
    geta cs0 ca0
    add cs1 cs0 1
    subseg ca0 cs0 cs1
    lea csp 1
    %fetch(2, ca1, cs0, cs1)
    %fetch(0, ct0, cs0, cs1)
    %fetch(1, ct1, cs0, cs1)
    mov cs0 cra
    jalr cra ct0
    mov cra cs0
    jalr cnull cra
B_g:
    ;; Both arguments are public objects; modifying either must not reach SO's
    ;; hidden secret immediately below the second object.
    lea ca0 -1
    store ca0 8
    store ca1 9
    jalr cnull cra
B_end:

StackObject_data:
StackObject_data_end:
B_data:
    #0
B_data_end:
SO_ext:
    #([X Ow LG LM], Global, StackObject, StackObject_end, StackObject)
    #([R W LG LM], Global, StackObject_data, StackObject_data_end, StackObject_data)
SO_ext_f: #(((StackObject_f - StackObject) << 3) || 2)
SO_ext_end:
B_ext:
    #([X Ow LG LM], Global, B, B_end, B)
    #([R W LG LM], Global, B_data, B_data_end, B_data)
B_ext_adv: #(((B_adv - B) << 3) || 0)
B_ext_g: #(((B_g - B) << 3) || 2)
B_ext_end:

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

%define ECOMPARTMENTFAIL -1
%define ENOTENOUGHTRUSTEDSTACK -141

;; Griotte trusted switcher.
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
    jmp (switcher_zero_stk_end_post - switcher_zero_stk_loop_post - 1)
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
