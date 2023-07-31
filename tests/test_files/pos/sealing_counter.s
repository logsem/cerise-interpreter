;;; Sealing counter example

;;; Defines a counter library using seal: the client has access to a global pointer for the counter
;;; via a sealed capability. The otype of this seal is required to be the same than the actual value
;;; of the counter to be valid.
;;; The call to `get` returns the current value of the counter, provided that the sealed pointer is valid.
;;; The call to `incr` increments both the value of the counter and the otype (the sealed pointer
;;; doesn't have to be valid).

;;; In this running example, the client checks that the initial counter is valid, then increments it
;;; 3 times, and finally checks that the counter is still valid.

init:
	;; prepare the sealed_counter in r1
	mov r0 pc             	; r0 = (RWX, init, end, init)
	mov r1 r0             	; r1 = (RWX, init, end, init)
	lea r1 (data-init)		; r1 = (RWX, init, end, data)
	load r1 r1				; r1 = {0: (RO, counter, counter+1, counter)}

	;; prepare the main capability in r0
	lea r0 (main+1-init)   	; r0 = (RWX, init, end, main+1)
	subseg r0 main data		; r0 = (RWX, main, data, main+1)
	restrict r0 E			; r0 = (E, main, data, main+1)
	jmp r0   				; jump to main main

main:
	;; r0 = pc / r1 = {0: (RO, counter, counter+1, counter)}
	#(RO, linking_table, end, linking_table)

	;; prepare the sentry for get and incr in r30 and r31
	mov r0 pc
	lea r0 (-1)				; r0 = (RX, main, data, main+1)
	load r31 r0				; r30 = (RO, linking_table, end, linking_table)
	load r30 r31 			; r30 = #(E, get, incr, get+1)
	lea r31 1
	load r31 r31 			; r31 = #(E, incr, end, incr+1)

	;; call get
	mov r0 pc
	lea r0 3				; r0 = callback
	jmp r30					; call get

	;; call incr 3 times
	mov r0 pc
	lea r0 3				; r0 = callback
	jmp r31					; call incr
	mov r0 pc
	lea r0 3				; r0 = callback
	jmp r31					; call incr
	mov r0 pc
	lea r0 3				; r0 = callback
	jmp r31					; call incr
	mov r0 pc

	;; call get
	mov r0 pc
	lea r0 3				; r0 = callback
	jmp r30					; call get

	halt
data:
	#{0: (RW, counter, counter+1, counter)}
linking_table:
	#(E, get, incr, get+1) 		; get
	#(E, incr, end, incr+2)		; incr
counter:
	#0
get: 							; check whether the otype matches with the actual value
	#[SU, 0, 10, 0]
	;; r0 contains callback / r1 = {ot: (RO, counter, counter+1, counter)}
	mov r2 pc 					; r2 = (RX, get, incr, get+1)
	lea r2 (-1)					; r2 = (RX, get, incr, get)
	load r2 r2					; r2 = #[SU, 0, 10, 0]
	getotype r3 r1				; r3 = ot
	lea r2 r3					; r2 = #[SU, 0, 10, ot]
	unseal r2 r2 r1				; r2 = (RO, counter, counter+1, counter)
	load r2 r2					; r2 = val_counter
	sub r3 r2 r3				; r3 = val_counter - ot
	mov r2 pc
	lea r2 5					; prepare jump to the fail instruction
	jnz r2 r3					; if r3 != 0, then fail, else return
	getotype r2 r1				; Case r3 = 0, then get_value and jmp to callback
	jmp r0						; r2 contains the return value
	fail 						; Case r3 != 0, then fail
incr:
	#[SU, 0, 10, 0]
	#(RW, incr, incr+1, incr)
	;; r0 contains callback / r1 = {ot: (RO, counter, counter+1, counter)}
	mov r2 pc 					; r2 = (RX, incr, end, incr+2)
	lea r2 (-2)					; r2 = (RX, incr, end, incr)
	load r3 r2					; r3 = #[SU, 0, 10, ot]
	unseal r1 r3 r1				; r1 = (RO, counter, counter+1, counter)
	load r4 r1					; r4 = val_counter
	add r4 r4 1					; r4 = val_counter + 1
	store r1 r4					; stores new counter value
	lea r3 1					; r3 = #[SU, 0, 10, ot+1]
	lea r2 1					; r2 = (RX, incr, end, incr+1)
	load r2 r2					; r2 = (RW, incr, incr+1, incr)
	store r2 r3					; stores new sealrange
	seal r1 r3 r1				; r1 = {ot+1: (RO, counter, counter+1, counter)}
	mov r2 0
	mov r3 0
	mov r4 0
	jmp r0
end:
