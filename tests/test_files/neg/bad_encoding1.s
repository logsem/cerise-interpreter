	mov r1 pc
	restrict r1 Sealed 			; FAIL: attempt to restrict SCap with a WType
	halt
