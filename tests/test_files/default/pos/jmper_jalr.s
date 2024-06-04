	mov r1 pc
	lea r1 5
	restrict r1 (E, GLOBAL)
	jalr r0 r1
	fail
	add r2 5 7
	halt
