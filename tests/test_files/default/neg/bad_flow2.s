	lea r3 4
	load r1 r3
	seal r1 r0 r1               ; FAIL: sentries cannot be sealed
	halt
# (E-[X Ow LG LM], Global, 0, 4, 0)
