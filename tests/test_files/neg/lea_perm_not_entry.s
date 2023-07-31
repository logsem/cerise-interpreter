	mov r1 pc
	restrict r1 E
	lea r1 15 					; FAIL: attempt to modify the address of a E-capability
	halt
