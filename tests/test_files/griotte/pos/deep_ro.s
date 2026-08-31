init:
    mov r1 cgp
    lea r1 (data-init)
    restrict r1 ([R W LG DRO], Global)
    load r1 r1
    load r2 r1
    halt
data:
	#([R W], Global, data, data+1, data)
	#([R W], Global, data+1, data+2, data+1)
