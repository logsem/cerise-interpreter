	mov r1 pc
	restrict r1 (E, GLOBAL)
	restrict r1 ([R W X], GLOBAL)			; FAIL: attempt to flow from E to RWX
	halt
