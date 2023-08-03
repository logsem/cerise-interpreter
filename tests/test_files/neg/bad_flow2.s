	mov r1 pc
	restrict r1 (O, GLOBAL)
	restrict r1 (E, GLOBAL) 				; FAIL: attempt to flow from O to E
	halt
