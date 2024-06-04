init:	
	mov r3 pc          			; r3 = (RX, init, end, init)			0
	mov r1 cgp          		; r1 = (RWL, init, end, init)			1
	lea r1 (data-init)			; r1 = (RWL, init, end, data)			2
	mov r2 r1             		; r2 = (RWL, init, end, data)			3
	lea r2 1              	 	; r2 = (RWL, init, end, data+1)			4
	store r2 2		         	; mem[data+1] <- 2 						5
	store r1 r2           		; mem[data] <- (RWL, init, end, data+1) 6
	lea r3 (code-init)			; r3 = (RX, init, end, code) 			7
	subseg r3 code end 			; r3 = (RX, code, end, code) 			8
	restrict r3 (E, GLOBAL)  	; r3 = (E, code, end, code) 			9
	mov r2 0 					; r2 = 0 								10
	mov r1 0 					; r1 = 0 								11
	jalr r2 r3   				; jump to unknown code:
								; we only give it access 				12

code:	
	mov r1 pc 					; r1 = (RX, code, end, code) 			13
	lea r1 (data-code)			; r1 = (RX, code, end, data) 			14
	load r1 r1					; r1 = (RWX, init, end, data+1) 		15
	load r2 r1 					; r2 = <counter value> 					16
	add r2 r2 1 				; r2 = <counter value>+1 				17
	store r1 r2					; mem[data+1] <- <counter value>+1 		18
	load r0 r1					; r0 = mem[data+1] 						19
	mov r1 0					; r1 = 0 								20
	halt 						; return to unknown code 				21
	
data:	
	mov r5 r5 					; 										22
	mov r5 0 					; 										23

end:	
