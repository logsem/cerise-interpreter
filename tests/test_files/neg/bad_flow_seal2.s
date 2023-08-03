	mov r1 pc
	restrict r1 (U, GLOBAL) 				; FAIL: attempt to restrict SCap with a Seal Permission
	halt
