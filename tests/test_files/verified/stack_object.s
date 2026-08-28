;;; Source: "Le Temps des Cerises" (OOPSLA 2022), directed-capabilities
;;; stack-object case study. Rocq definitions:
;;;   theories/examples/stack_object.v (stack_object_passing_instrs)
;;;   theories/examples/stack_object_preamble.v
;;;   theories/examples/stack_object_adequacy.v
;;; This runnable instance retains the complete preamble, link table, malloc
;;; and assertion routines, dynamic argument checks, directed-stack scallU
;;; convention, private-state assertion, register clearing, and return.
;;; The adequacy layout places this linking-table pointer at PC offset zero.
linking_pointer:
	# (RO, Global, link_table, link_table_end, link_table)

start:
stack_object_preamble:
;;; Exact stack_object_preamble_instrs: create the exported object entry in
;;; r_t1 and hand control to the caller supplied in r_t0 by the register file.
	mov r1 pc
	lea r1 (stack_object_body - &CURRENT_ADDR + 1)
	restrict r1 (E, Global)
	jmp r0

stack_object_body:
;;; Exact expansion of stack_object_passing_instrs 42 1 begins here.
	getl r1 r29
	sub r1 r1 18
	mov r2 pc
	lea r2 6
	jnz r2 r1
	mov r2 pc
	lea r2 4
	jmp r2
	fail
	mov r1 0
	mov r2 0
	getp r1 stk
	sub r1 r1 248
	mov r2 pc
	lea r2 6
	jnz r2 r1
	mov r2 pc
	lea r2 4
	jmp r2
	fail
	mov r1 0
	mov r2 0
	getb r1 stk
	gete r2 stk
	sub r1 r2 r1
	lt r1 14 r1
	mov r2 pc
	lea r2 4
	jnz r2 r1
	fail
	getb r1 stk
	geta r2 stk
	sub r2 r2 2
	sub r1 r1 r2
	lea stk r1
	mov r1 0
	mov r2 0
	loadU r16 stk -1
;;; checkra_instrs r_param1.
	mov r3 pc
	lea r3 29
	getp r1 r16
	sub r2 r1 32
	mov r4 pc
	lea r4 4
	jnz r4 r2
	jmp r3
	sub r2 r1 40
	lea r4 4
	jnz r4 r2
	jmp r3
	sub r2 r1 48
	lea r4 4
	jnz r4 r2
	jmp r3
	sub r2 r1 112
	lea r4 4
	jnz r4 r2
	jmp r3
	sub r2 r1 56
	lea r4 4
	jnz r4 r2
	jmp r3
	sub r2 r1 120
	lea r4 4
	jnz r4 r2
	jmp r3
	fail
	mov r1 0
	mov r2 0
	mov r3 0
	mov r4 0
;;; checkints_instrs r_param1 r_t1 r_t2 r_t3. The historical is_ptr
;;; instruction is represented by getwtype followed by the W_Cap test (11).
	getb r1 r16
	geta r2 r16
	sub r1 r1 r2
	lea r16 r1
	mov r1 0
	mov r2 0
	getb r1 r16
	gete r2 r16
	mov r3 pc
	lea r3 22
	lt r1 r1 r2
	sub r1 r1 1
	jnz r3 r1
	load r1 r16
	getwtype r3 r1
	sub r3 r3 11
	mov r2 pc
	lea r2 4
	jnz r2 r3
	fail
	mov r2 0
	mov r3 0
	geta r1 r16
	add r1 r1 1
	gete r2 r16
	lt r1 r1 r2
	mov r2 pc
	lea r2 -13
	lea r16 1
	jnz r2 r1
	mov r1 0
	mov r2 0
	mov r3 0
;;; Create the private value 2 and the directed stack object containing 42.
	storeU stk 0 2
	storeU stk 0 42
	mov r17 stk
	getb r1 stk
	gete r2 stk
	add r1 r1 3
	subseg r17 r1 r2
	promoteU r17
;;; Exact scallU_prologue_instrs r_adv [r_param1; r_param2].
	storeU stk 0 4098
	storeU stk 0 45088
	storeU stk 0 1056772
	storeU stk 0 135182
	storeU stk 0 68719484977
	mov r1 pc
	lea r1 44
	storeU stk 0 r1
	storeU stk 0 stk
	mov ddc stk
	promoteU ddc
	lea ddc -7
	restrict ddc (E, Directed)
	geta r1 stk
	gete r2 stk
	subseg stk r1 r2
	mov r1 0
	mov r2 0
	mov r3 0
	mov r4 0
	mov r5 0
	mov r6 0
	mov r7 0
	mov r8 0
	mov r9 0
	mov r10 0
	mov r11 0
	mov r12 0
	mov r13 0
	mov r14 0
	mov r15 0
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
	mov r30 0
;;; Pass both capabilities and the protected return on the adversary stack.
	storeU stk 0 r16
	storeU stk 0 r17
	storeU stk 0 ddc
	mov r16 0
	mov r17 0
	mov ddc 0
	jmp r29
;;; scallU continuation: restore the private frame and assert its value is 2.
	lea stk -6
	loadU r29 stk -2
	mov r1 pc
	getb r2 r1
	geta r3 r1
	sub r2 r2 r3
	lea r1 r2
	load r1 r1
	lea r1 1
	mov r2 0
	mov r3 0
	load r1 r1
	sub r29 r29 2
	jnz r1 r29
	mov r1 0
	loadU r1 stk -4
;;; rclear_instrs (all_registers minus [PC; r_t1]); return via r_t1.
	mov ddc 0
	mov r2 0
	mov r3 0
	mov r4 0
	mov r5 0
	mov r6 0
	mov r7 0
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
	mov r30 0
	mov stk 0
	jmp r1
stack_object_region_end:

example_caller:
;;; EXAMPLE ADVERSARY/CALLER. This is one concrete inhabitant of the paper's
;;; arbitrary caller: it uses the exported entry capability in r1, supplies a
;;; readable integer object, and designates further arbitrary code in r29.
	mov r16 pc
	lea r16 (caller_object - &CURRENT_ADDR + 1)
	subseg r16 caller_object caller_object_end
	restrict r16 (RO, Global)
	mov r29 pc
	lea r29 (nested_adversary - &CURRENT_ADDR + 1)
	subseg r29 nested_adversary nested_adversary_end
	restrict r29 (E, Global)
	mov r0 pc
	lea r0 (caller_return - &CURRENT_ADDR + 1)
	subseg r0 caller_return caller_return_end
	restrict r0 (E, Global)
	storeU stk 0 r0
	storeU stk 0 r16
	jmp r1

nested_adversary:
;;; EXAMPLE ADVERSARY (it could be arbitrary code). It actively uses every
;;; capability handed to it on the restricted stack: it retrieves and reads
;;; the checked object, reads the deliberately exposed value through the
;;; directed stack object, then retrieves and invokes the protected return.
	loadU r16 stk -3
	loadU r17 stk -2
	lea r16 -1
	load r18 r16
	lea r17 -1
	load r19 r17
	storeU stk 0 r18
	storeU stk 0 r19
	loadU ddc stk -3
	jmp ddc
nested_adversary_end:

caller_return:
	halt
caller_return_end:

caller_object:
	# 7
	# 11
	# 13
caller_object_end:
example_caller_end:

;;; Source malloc_subroutine_instrs and its bump-pointer pool.
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
	# (RWX, Global, malloc_cursor, malloc_end, malloc_pool)
malloc_pool:
	# 0
	# 0
	# 0
	# 0
malloc_end:

;;; Source assert_fail_instrs. Its flag capability is exactly six words after
;;; the routine's first instruction, as required by the verified convention.
assert_fail:
	mov r1 pc
	lea r1 6
	load r1 r1
	store r1 1
	mov r1 0
	fail
assert_flag_cap:
	# (RW, Global, assert_flag, assert_flag_end, assert_flag)
assert_fail_end:
assert_flag:
	# 0
assert_flag_end:

;;; Adequacy linking table: malloc at offset 0, assertion failure at offset 1.
link_table:
	# (E, Global, malloc, malloc_end, malloc)
	# (E, Global, assert_fail, assert_fail_end, assert_fail)
link_table_end:
