init:
	mov r1 pc             	; r1 = (RWX, init, end, init) 0
	lea r1 (data-init)	; r1 = (RWX, init, end, data) 1
	mov r2 r1             	; r2 = (RWX, init, end, data) 2
	lea r2 1              	; r2 = (RWX, init, end, data+1) 3
	store r2 2		; mem[data+1] <- 2 4
	store r1 r2           	; mem[data] <- (RWX, init, end, data+1) 5
	lea r1 (code-data)	; r1 = (RWX, init, end, code) 6
	subseg r1 code end 	; r1 = (RWX, code, end, code) 7
	restrict r1 E 		; r1 = (E, code, end, code) 8
	seal r2 r0 r2       ; r2 = {0, (RWX, init, end, data+1)}
	store pc r2
	store pc pc
	lea r0 100000
	seal r2 r0 r0
	jmp r1   		; jump to unknown code: we only give it access 10

code:
	unseal r2 r0 r2       ; r2 = (RWX, init, end, data+1)
	mov r1 pc 		; r1 = (RX, code, end, code) 11
	lea r1 (data-code)	; r1 = (RX, code, end, data) 12
	load r1 r1		; r1 = (RWX, init, end, data+1) 13
	halt 			; return to unknown code 19

data:
	#(RO, data+1, data+2, data+1)
	#{0: (RW, data+2, data+3, data+2)}
	#[O, 0, 0, 0]
end:
