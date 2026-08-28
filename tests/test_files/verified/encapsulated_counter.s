;;; Source: Cerise journal paper (JACM 2024), Section 2.4, "a counter
;;; compartment"; Rocq: theories/examples/counter.v and counter_preamble.v.
;;; This runnable companion implements the source interface: the preamble
;;; dynamically allocates a private cell and returns three heap closures for
;;; incr, read, and reset. The closures use crtcls_instrs activation records.
start:
	mov r1 1
	mov r0 pc
	lea r0 (after_state - &CURRENT_ADDR + 1)
	subseg r0 after_state after_state_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (malloc - &CURRENT_ADDR + 1)
	subseg r30 malloc counter_library_end
	restrict r30 (E, Global)
	jmp r30
after_state:
	store r1 0
	mov r15 r1
;;; Create the exported incr closure.
	mov r1 pc
	lea r1 (incr - &CURRENT_ADDR + 1)
	subseg r1 incr counter_code_end
	restrict r1 (RX, Global)
	mov r2 r15
	mov r0 pc
	lea r0 (after_incr_closure - &CURRENT_ADDR + 1)
	subseg r0 after_incr_closure after_incr_closure_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (crtcls - &CURRENT_ADDR + 1)
	subseg r30 crtcls counter_library_end
	restrict r30 (E, Global)
	jmp r30
after_incr_closure:
	mov r16 r1
;;; Create the exported read closure.
	mov r1 pc
	lea r1 (read - &CURRENT_ADDR + 1)
	subseg r1 read counter_code_end
	restrict r1 (RX, Global)
	mov r2 r15
	mov r0 pc
	lea r0 (after_read_closure - &CURRENT_ADDR + 1)
	subseg r0 after_read_closure after_read_closure_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (crtcls - &CURRENT_ADDR + 1)
	subseg r30 crtcls counter_library_end
	restrict r30 (E, Global)
	jmp r30
after_read_closure:
	mov r17 r1
;;; Create the exported reset closure. It shares the same private environment.
	mov r1 pc
	lea r1 (reset - &CURRENT_ADDR + 1)
	subseg r1 reset counter_code_end
	restrict r1 (RX, Global)
	mov r2 r15
	mov r0 pc
	lea r0 (after_reset_closure - &CURRENT_ADDR + 1)
	subseg r0 after_reset_closure after_reset_closure_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (crtcls - &CURRENT_ADDR + 1)
	subseg r30 crtcls counter_library_end
	restrict r30 (E, Global)
	jmp r30
after_reset_closure:
	mov r18 r1
;;; Client calls incr and read through entry capabilities; r31 receives value.
	mov r0 pc
	lea r0 (after_incr - &CURRENT_ADDR + 1)
	subseg r0 after_incr after_incr_end
	restrict r0 (E, Global)
	jmp r16
after_incr:
	mov r0 pc
	lea r0 (after_read - &CURRENT_ADDR + 1)
	subseg r0 after_read after_read_end
	restrict r0 (E, Global)
	jmp r17
after_read:
	mov r2 r31
	halt
after_state_end:
after_incr_closure_end:
after_read_closure_end:
after_reset_closure_end:
after_incr_end:
after_read_end:

;;; Verified counter bodies. r30 is r_env and r0 is r_t0.
incr:
	load r1 r30
	add r1 r1 1
	store r30 r1
	mov r30 0
	jmp r0
read:
	load r31 r30
	lt r4 r31 0
	mov r5 0
	mov r19 r0
	mov r0 pc
	lea r0 (read_after_assert - &CURRENT_ADDR + 1)
	subseg r0 read_after_assert read_after_assert_end
	restrict r0 (E, Global)
	mov r6 pc
	lea r6 (assert_equal - &CURRENT_ADDR + 1)
	subseg r6 assert_equal assert_equal_end
	restrict r6 (E, Global)
	jmp r6
read_after_assert:
	mov r30 0
	mov r0 r19
	mov r19 0
	jmp r0
read_after_assert_end:
reset:
	store r30 0
	mov r30 0
	mov r1 0
	jmp r0
counter_code_end:

;;; crtcls_instrs from macros_new.v. Inputs r1=code, r2=environment, r0=return.
crtcls:
	mov r6 r1
	mov r7 r2
	mov r5 r0
	mov r1 8
	mov r0 pc
	lea r0 (crtcls_after_malloc - &CURRENT_ADDR + 1)
	subseg r0 crtcls_after_malloc crtcls_after_malloc_end
	restrict r0 (E, Global)
	mov r8 pc
	lea r8 (malloc - &CURRENT_ADDR + 1)
	subseg r8 malloc counter_library_end
	restrict r8 (E, Global)
	jmp r8
crtcls_after_malloc:
	store r1 279554
	lea r1 1
	store r1 322592
	lea r1 1
	store r1 908292
	lea r1 1
	store r1 282144
	lea r1 1
	store r1 838660
	lea r1 1
	store r1 5376
	lea r1 1
	store r1 r6
	mov r6 0
	lea r1 1
	store r1 r7
	mov r7 0
	lea r1 -7
	restrict r1 (E, Global)
	mov r0 r5
	mov r5 0
	jmp r0
crtcls_after_malloc_end:

;;; Bump-pointer malloc service. Its cursor is reached through an explicit RW cap.
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
# 0
# 0
# 0
# 0
# 0
# 0
# 0
# 0
malloc_pool_end:
counter_library_end:
