	mov r1 pc
	restrict r1 SU              ; FAIL: attempt to restrict SCap with a Seal Permission
	halt