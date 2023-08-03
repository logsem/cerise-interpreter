	mov r1 pc
	restrict r1 (E, GLOBAL)
	restrict r1 (RWX, GLOBAL)			; FAIL: attempt to flow from E to RWX
	halt
