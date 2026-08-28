;;; Source: "Efficient and Provable Local Capability Revocation using
;;; Uninitialized Capabilities" (POPL 2021), Section 6, "Awkward Example".
;;; Rocq: theories/examples/awkward_example.v. The body below is the exact
;;; expansion of Scall_ucap.awkward_example from the interpreter's source.
%macro clear_call()
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
	mov r29 0
	mov r30 0
%endmacro
start:
;;; The initial register file supplies the source-level initial state: r30 is
;;; the private environment, r28 the adversary entry, and r0 the outer return.
;;; reqglob_instrs r28
	getl r1 r28
	sub r1 r1 18
	mov r2 pc
	lea r2 r1
	jnz r2 r1
	mov r2 pc
	lea r2 4
	jmp r2
	fail
	mov r1 0
	mov r2 0
;;; prepstack_instrs stk 10
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
	mov r1 0
	getb r1 stk
	gete r2 stk
	sub r1 r2 r1
	lt r1 10 r1
	mov r2 pc
	lea r2 4
	jnz r2 r1
	fail
	getb r1 stk
	geta r2 stk
	sub r1 r1 r2
	lea stk r1
	mov r1 0
	mov r2 0
;;; First source-level scoped adversary call.
	store r30 0
	storeU stk 0 r30
	storeU stk 0 r0
	storeU stk 0 r28
	storeU stk 0 4098
	storeU stk 0 45088
	storeU stk 0 1056772
	storeU stk 0 135182
	storeU stk 0 68719484977
	mov r1 pc
	lea r1 40
	storeU stk 0 r1
	storeU stk 0 stk
	mov r0 stk
	promoteU r0
	lea r0 -7
	restrict r0 (E, Local)
	geta r1 stk
	gete r2 stk
	subseg stk r1 r2
%clear_call()
	jmp r28
	storeU stk 0 0
	sub r1 0 7
	lea stk r1
	loadU r28 stk -1
	sub r1 0 1
	lea stk r1
	loadU r0 stk -1
	sub r1 0 1
	lea stk r1
	loadU r30 stk -1
	sub r1 0 1
	lea stk r1
;;; Second scoped call, after the private environment changes to 1.
	store r30 1
	storeU stk 0 r30
	storeU stk 0 r0
	storeU stk 0 4098
	storeU stk 0 45088
	storeU stk 0 1056772
	storeU stk 0 135182
	storeU stk 0 68719484977
	mov r1 pc
	lea r1 40
	storeU stk 0 r1
	storeU stk 0 stk
	mov r0 stk
	promoteU r0
	lea r0 -7
	restrict r0 (E, Local)
	geta r1 stk
	gete r2 stk
	subseg stk r1 r2
%clear_call()
	jmp r28
	sub r1 0 6
	lea stk r1
	loadU r0 stk -1
	sub r1 0 1
	lea stk r1
	loadU r30 stk -1
	sub r1 0 1
	lea stk r1
;;; mclearU then rclear: clear the revoked temporary frame before returning.
	getb r1 stk
	add r2 r1 r2
	subseg stk r1 r2
	mov r4 stk
	getb r1 r4
	getb r2 r4
	sub r2 r1 r2
	lea r4 r2
	gete r5 r4
	sub r5 r5 1
	mov r2 pc
	lea r2 9
	mov r3 pc
	lea r3 2
	lt r6 r5 r1
	jnz r2 r6
	storeU r4 0 0
	add r1 r1 1
	jmp r3
	mov r1 0
	mov r2 0
	mov r3 0
	mov r4 0
	mov r5 0
	mov r6 0
	mov stk 0
%clear_call()
	mov r28 0
	mov stk 0
	jmp r0
example_adversary:
;;; Example adversary: the source adv_instr returns through the protected entry
;;; capability in r0; arbitrary code could run before this instruction.
	jmp r0
example_adversary_end:
outer_return:
	halt
outer_return_end:
environment:
# 0
environment_end:
