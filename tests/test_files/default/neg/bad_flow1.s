	lea r3 4
	load r1 r3
	unseal r1 r0 r1             ; FAIL: sentries cannot be unsealed
	halt
# (E-[X Ow LG LM], Global, 0, 4, 0)
