	mov r1 pc
	restrict r1 (E, GLOBAL)
	lea r1 15 					; FAIL: attempt to modify the address of a E-capability
	halt
