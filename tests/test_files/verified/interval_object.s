;;; Source: Cerise journal paper (JACM 2024), Section 8.1, "Interval Library
;;; and Client". Rocq: theories/examples/interval.v and interval_closure.v.
;;; The library exports heap closures makeint, imin, and imax. Its dynamically
;;; allocated environment contains the fresh sealing capability they share.
start:
;;; Allocate the two-word library environment and obtain a fresh sealing range.
	;;; The initial register file supplies size 2 in r1, the continuation in r0,
	;;; and the malloc entry capability in r30.
	jmp r30
after_env:
	mov r15 r1
	mov r0 pc
	lea r0 (after_make_seal - &CURRENT_ADDR + 1)
	subseg r0 after_make_seal after_make_seal_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (make_seal - &CURRENT_ADDR + 1)
	subseg r30 make_seal interval_library_end
	restrict r30 (E, Global)
	jmp r30
after_make_seal:
	store r15 r2
	lea r15 1
	store r15 r2
	lea r15 -1
;;; interval_closure creates one closure for each public operation.
	mov r1 pc
	lea r1 (makeint - &CURRENT_ADDR + 1)
	subseg r1 makeint interval_code_end
	restrict r1 (RX, Global)
	mov r2 r15
	mov r0 pc
	lea r0 (after_makeint_closure - &CURRENT_ADDR + 1)
	subseg r0 after_makeint_closure after_makeint_closure_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (crtcls - &CURRENT_ADDR + 1)
	subseg r30 crtcls interval_library_end
	restrict r30 (E, Global)
	jmp r30
after_makeint_closure:
	mov r16 r1
	mov r1 pc
	lea r1 (imin - &CURRENT_ADDR + 1)
	subseg r1 imin interval_code_end
	restrict r1 (RX, Global)
	mov r2 r15
	mov r0 pc
	lea r0 (after_imin_closure - &CURRENT_ADDR + 1)
	subseg r0 after_imin_closure after_imin_closure_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (crtcls - &CURRENT_ADDR + 1)
	subseg r30 crtcls interval_library_end
	restrict r30 (E, Global)
	jmp r30
after_imin_closure:
	mov r17 r1
	mov r1 pc
	lea r1 (imax - &CURRENT_ADDR + 1)
	subseg r1 imax interval_code_end
	restrict r1 (RX, Global)
	mov r2 r15
	mov r0 pc
	lea r0 (after_imax_closure - &CURRENT_ADDR + 1)
	subseg r0 after_imax_closure after_imax_closure_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (crtcls - &CURRENT_ADDR + 1)
	subseg r30 crtcls interval_library_end
	restrict r30 (E, Global)
	jmp r30
after_imax_closure:
	mov r18 r1
;;; Client: makeint 42 10 normalizes to (10,42), then calls imax.
	mov r1 42
	mov r2 10
	mov r0 pc
	lea r0 (after_makeint - &CURRENT_ADDR + 1)
	subseg r0 after_makeint after_makeint_end
	restrict r0 (E, Global)
	jmp r16
after_makeint:
	mov r0 pc
	lea r0 (after_imax - &CURRENT_ADDR + 1)
	subseg r0 after_imax after_imax_end
	restrict r0 (E, Global)
	jmp r18
after_imax:
	;;; imax returns the normalized upper endpoint in r2.
	halt
after_env_end:
after_make_seal_end:
after_makeint_closure_end:
after_imin_closure_end:
after_imax_closure_end:
after_makeint_end:
after_imax_end:

;;; makeint from interval.v: allocate a pair, order its endpoints, and seal it.
makeint:
	mov r6 r1
	mov r7 r2
	mov r5 r0
	mov r1 2
	mov r0 pc
	lea r0 (makeint_after_malloc - &CURRENT_ADDR + 1)
	subseg r0 makeint_after_malloc makeint_after_malloc_end
	restrict r0 (E, Global)
	mov r8 pc
	lea r8 (malloc - &CURRENT_ADDR + 1)
	subseg r8 malloc interval_library_end
	restrict r8 (E, Global)
	jmp r8
makeint_after_malloc:
	lt r3 r6 r7
	mov r4 pc
	lea r4 (makeint_no_swap - &CURRENT_ADDR + 1)
	jnz r4 r3
	mov r4 r6
	mov r6 r7
	mov r7 r4
makeint_no_swap:
	store r1 r6
	lea r1 1
	store r1 r7
	lea r1 -1
	load r31 r30
	seal r1 r31 r1
	mov r0 r5
	mov r5 0
	mov r6 0
	mov r7 0
	jmp r0
makeint_after_malloc_end:

;;; imin and imax unseal their argument with the private environment range.
imin:
	load r31 r30
	unseal r1 r31 r1
	load r2 r1
	mov r30 0
	jmp r0
imax:
	load r31 r30
	unseal r1 r31 r1
	lea r1 1
	load r2 r1
	mov r30 0
	jmp r0
interval_code_end:

;;; crtcls_instrs from macros_new.v: allocate and initialize a heap closure.
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
	subseg r8 malloc interval_library_end
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

;;; Fresh dynamic sealing range, corresponding to the seal environment setup.
make_seal:
	mov r8 pc
	lea r8 (seal_cursor_cap - &CURRENT_ADDR + 1)
	load r8 r8
	load r2 r8
	lea r2 1
	store r8 r2
	lea r2 -1
	jmp r0
;;; Bump-pointer malloc used by malloc_instrs and crtcls.
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
malloc_cursor:
#(RWX, Global, malloc_cursor, malloc_pool_end, malloc_pool)
seal_cursor:
#[SU, Global, 0, 10, 0]
seal_cursor_end:
seal_cursor_cap:
#(RW, Global, seal_cursor, seal_cursor_end, seal_cursor)
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
interval_library_end:
