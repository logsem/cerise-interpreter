;;; Source: "Le Temps des Cerises" (OOPSLA 2022), downward local-state
;;; encapsulation. Rocq: theories/examples/downwards_lse.v (lse_instrs).
;;; The body is the source sequence: prepstackU, retrieve the stack return,
;;; push and dereference r_env, assert 2, clear registers, and return.
start:
;;; The initial register file supplies the private environment in r30 and the
;;; outer return in r0. The one remaining setup instruction passes that return
;;; as prepstackU's stack parameter.
	storeU stk 0 r0
;;; prepstackU_instrs stk 1 1 (permission and size checks, then frame setup).
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
	lt r1 2 r1
	mov r2 pc
	lea r2 4
	jnz r2 r1
	fail
	getb r1 stk
	geta r2 stk
	sub r2 r2 1
	sub r1 r1 r2
	lea stk r1
	mov r1 0
	mov r2 0
;;; loadU r_t0 r_stk (-1); pushU r_stk r_env; load r_env r_env.
	loadU r0 stk -1
	storeU stk 0 r30
	load r30 r30
;;; assert_r_z_instrs f_a r_env 2, with its successful branch inlined.
	sub r30 r30 2
	mov r1 pc
	lea r1 (assert_failed - &CURRENT_ADDR + 1)
	jnz r1 r30
	mov r1 0
;;; rclear_instrs (all registers except PC and r_t0), then jmp r_t0.
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
	mov r28 0
	mov r29 0
	mov r30 0
	mov r31 0
	mov stk 0
	jmp r0
assert_failed:
	fail
outer_return:
	halt
outer_return_end:
private_state:
# 2
private_state_end:
