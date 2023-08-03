	mov r1 pc
	restrict r1 O
	restrict r1 E 				; FAIL: attempt to flow from O to E
	halt
