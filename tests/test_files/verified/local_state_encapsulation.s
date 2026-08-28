;;; Source: Cerise journal paper (JACM 2024), Section 6.2, "Local State
;;; Encapsulation". Rocq: theories/examples/lse.v (roe_instrs).
;;; The source allocates a private cell, shares only an RO restriction with an
;;; arbitrary adversary, and uses call_instrs to save/restore the local env.
start:
;;; malloc_instrs f_m 1: allocate the private component cell.
	mov r1 1
	mov r0 pc
	lea r0 (after_malloc - &CURRENT_ADDR + 1)
	subseg r0 after_malloc after_malloc_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (malloc - &CURRENT_ADDR + 1)
	subseg r30 malloc lse_library_end
	restrict r30 (E, Global)
	jmp r30
after_malloc:
	;;; move r_env r_t1; move r_t7 r_t1; store r_env 1; restrict r_t7 RO.
	mov r15 r1
	mov r7 r1
	store r15 1
	restrict r7 (RO, Global)
;;; call_instrs saves r_env in a local one-word environment before control
;;; crosses to the adversary.
	mov r1 1
	mov r0 pc
	lea r0 (after_locals - &CURRENT_ADDR + 1)
	subseg r0 after_locals after_locals_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (malloc - &CURRENT_ADDR + 1)
	subseg r30 malloc lse_library_end
	restrict r30 (E, Global)
	jmp r30
after_locals:
	mov r6 r1
	store r6 r15
;;; Allocate the seven-word call activation record and install the exact
;;; encoded return sequence used by call_instrs.
	mov r1 7
	mov r0 pc
	lea r0 (after_activation_allocation - &CURRENT_ADDR + 1)
	subseg r0 after_activation_allocation after_activation_allocation_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (malloc - &CURRENT_ADDR + 1)
	subseg r30 malloc lse_library_end
	restrict r30 (E, Global)
	jmp r30
after_activation_allocation:
	store r1 4098
	lea r1 1
	store r1 38944
	lea r1 1
	store r1 13316
	lea r1 1
	store r1 6176
	lea r1 1
	store r1 8196
	lea r1 1
	store r1 r6
	lea r1 1
	mov r8 pc
	lea r8 (after_adversary - &CURRENT_ADDR + 1)
	subseg r8 after_adversary after_adversary_end
	restrict r8 (RX, Global)
	store r1 r8
	lea r1 -6
	restrict r1 (E, Global)
	mov r0 r1
;;; r7 is the sole public argument. Clear all private temporary registers.
	mov r1 0
	mov r2 0
	mov r3 0
	mov r4 0
	mov r5 0
	mov r6 0
	mov r8 0
	mov r9 0
	mov r10 0
	mov r11 0
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
	mov r30 pc
	lea r30 (example_adversary - &CURRENT_ADDR + 1)
	subseg r30 example_adversary example_adversary_end
	restrict r30 (E, Global)
	jmp r30
after_adversary:
;;; restore_locals_instrs restores r_env in r2. Assert its cell still is 1.
	load r4 r2
	mov r5 1
	mov r19 r0
	mov r0 pc
	lea r0 (after_assert - &CURRENT_ADDR + 1)
	subseg r0 after_assert after_assert_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (assert_equal - &CURRENT_ADDR + 1)
	subseg r30 assert_equal assert_equal_end
	restrict r30 (E, Global)
	jmp r30
after_assert:
	;;; Reaching here witnesses the assertion. Keep the source postcondition in r2.
	mov r2 1
	halt
after_malloc_end:
after_locals_end:
after_activation_allocation_end:
after_adversary_end:
after_assert_end:

;;; Example adversary only. The theorem permits arbitrary code; this concrete
;;; one exercises the RO capability, but has no write or private local cap.
example_adversary:
	getp r3 r7
	load r4 r7
	add r5 r4 41
	jmp r0
example_adversary_end:

;;; Bump-pointer implementation for malloc_instrs.
malloc:
	mov r8 pc
	lea r8 (malloc_cursor - &CURRENT_ADDR + 1)
	load r8 r8
	mov r13 r1
	geta r10 r8
	mov r9 r10
	add r9 r9 r13
	mov r1 r8
	subseg r1 r10 r9
	mov r12 r8
	lea r12 r13
	getb r11 r8
	sub r11 r11 r10
	lea r8 r11
	store r8 r12
	jmp r0
;;; assert_instrs' trusted target: r4 and r5 must agree.
assert_equal:
	sub r8 r4 r5
	mov r9 pc
	lea r9 (assert_failed - &CURRENT_ADDR + 1)
	jnz r9 r8
	mov r4 0
	mov r5 0
	jmp r0
assert_failed:
	fail
assert_equal_end:
malloc_cursor:
#(RWX, Global, malloc_cursor, malloc_pool_end, malloc_pool)
malloc_pool:
# 0
# 0
# 0
# 0
# 0
# 0
# 0
# 0
# 0
# 0
# 0
# 0
malloc_pool_end:
lse_library_end:
