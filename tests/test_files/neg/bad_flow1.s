	mov r1 pc
	restrict r1 E
	restrict r1 RWX				; FAIL: attempt to flow from E to RWX
	halt
