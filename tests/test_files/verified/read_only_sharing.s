;;; Source: Cerise journal paper (JACM 2024), Section 7.5,
;;; "read-only sharing of dynamically allocated memory".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/ocpl_lowval_like.v
;;; Runnable companion to the journal paper's verified read-only-sharing example.
;;; It uses the journal malloc, assert, and heap activation-record call pattern.
start:
;;; Call the bump-pointer malloc subroutine. r1 is the requested size and r0
;;; is its entry return capability; malloc returns the RW capability in r1.
;;; The initial register file supplies r1, r0, and the malloc entry in r30.
jmp r30

after_malloc:
;;; Initialize the private RW capability, then share only its RO restriction.
store r1 1
mov r15 r1
mov r7 r1
restrict r7 (RO, Global)
;;; Allocate and initialize the one-word local environment saved across the
;;; adversary call, as in call_instrs with locals=[r_env].
mov r1 1
mov r0 pc
lea r0 (after_locals - &CURRENT_ADDR + 1)
subseg r0 after_locals after_locals_end
restrict r0 (E, Global)
mov r30 pc
lea r30 (malloc - &CURRENT_ADDR + 1)
subseg r30 malloc malloc_library_end
restrict r30 (E, Global)
jmp r30

after_locals:
mov r6 r1
store r6 r15
;;; Allocate the seven-word heap activation record used by the calling
;;; convention, then fill it with the source activation sequence.
mov r1 7
mov r0 pc
lea r0 (after_activation_allocation - &CURRENT_ADDR + 1)
subseg r0 after_activation_allocation after_activation_allocation_end
restrict r0 (E, Global)
mov r30 pc
lea r30 (malloc - &CURRENT_ADDR + 1)
subseg r30 malloc malloc_library_end
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
;;; Store the continuation after the call, then turn the record into an entry
;;; capability. The encoded instructions above restore r2 from r6 and jump here.
mov r8 pc
lea r8 (after_adversary - &CURRENT_ADDR + 1)
subseg r8 after_adversary after_adversary_end
restrict r8 (RX, Global)
store r1 r8
lea r1 -6
restrict r1 (E, Global)
mov r0 r1
;;; r7 is the sole public argument. Other temporary registers are cleared
;;; before control crosses to the adversary.
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
mov r31 0
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (E, Global)
jmp r30

after_adversary:
;;; The original RW capability is private. Check that its cell still contains
;;; 1 by calling the assert subroutine with r4 and r5 as its arguments.
load r4 r2
mov r5 1
mov r0 pc
lea r0 (after_assert - &CURRENT_ADDR + 1)
subseg r0 after_assert after_assert_end
restrict r0 (E, Global)
mov r30 pc
lea r30 (assert_equal - &CURRENT_ADDR + 1)
subseg r30 assert_equal assert_library_end
restrict r30 (E, Global)
jmp r30

after_assert:
;;; Observation point for the runnable companion: reaching this point means the
;;; source assertion established that the private cell still contains 1.
	mov r4 1
	halt
after_malloc_end:
after_locals_end:
after_activation_allocation_end:
after_adversary_end:
after_assert_end:

;;; Example adversary only: it can read through r7, but it cannot write. It
;;; returns through the entry capability in r0, which restores the saved local.
example_adversary:
getp r3 r7
load r2 r7
jmp r0
example_adversary_end:

;;; Journal malloc_subroutine_instrs. Its cursor is stored immediately after
;;; the code, as required by the source routine's PC-relative load.
malloc:
lt r3 0 r1
mov r2 pc
lea r2 4
jnz r2 r3
fail
mov r2 pc
lea r2 21
load r2 r2
geta r3 r2
lea r2 r1
geta r1 r2
mov r4 r2
subseg r4 r3 r1
sub r3 r3 r1
lea r4 r3
mov r3 r2
sub r1 0 r1
lea r3 r1
getb r1 r3
lea r3 r1
store r3 r2
mov r1 r4
mov r2 0
mov r3 0
mov r4 0
jmp r0
malloc_cursor:
#(RWX, Global, malloc_cursor, malloc_pool_end, malloc_pool)
malloc_end:

;;; Journal assert_subroutine_instrs. The capability for assert_flag follows
;;; the code at the source-specified offset.
assert_equal:
sub r4 r4 r5
mov r5 pc
lea r5 6
jnz r5 r4
mov r4 0
mov r5 0
jmp r0
assert_failed:
lea r5 6
load r5 r5
store r5 1
mov r4 0
mov r5 0
jmp r0

assert_flag_cap:
#(RW, Global, assert_flag, assert_library_end, assert_flag)
assert_flag:
# 0
assert_library_end:
malloc_pool:
# 0
# 0
# 0
# 0
malloc_pool_end:
malloc_library_end:
