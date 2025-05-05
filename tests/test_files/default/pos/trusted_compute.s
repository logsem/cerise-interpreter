boot:
    mov r0 pc
    mov r1 r0
    getb r2 r0
    ; create adv
    add r3 r2 (adv_start-boot)
    add r4 r2 (adv_end-boot)
    subseg r0 r3 r4
    lea r0 r3

    ; create main
    add r3 r2 (main-boot)
    add r4 r2 (main_end-boot)
    subseg r1 r3 r4
    lea r1 r3

    jmp r1


main:
   ; create callback sentry
   mov r1 pc
   lea r1 (callback - main)
   restrict r1 (E, Global)

   ; jump to adversary
   jmp r0

callback:
   ; PC := (RX, main, main_end, callback)
   ; r0 := <expects sealed value>
   ; r1 := <expects unsealing value>

   ; r3 contains the failing capability, if any check fails
   mov r3 pc
   mov r4 r3
   lea r3 (fails - callback)

   ; check that r0 contains a capability
   getotype r2 r0
   sub r2 r2 (-1)
   mov r5 pc
   lea r5 4
   jnz r5 r2
   jmp r3

   ; attestation
   getotype r2 r0
   estoreid r4 r2
   ; check otype(w_res) against identity of the enclave
   sub r4 r4 0x2C771E89
   jnz r3 r4

   ; get returned value and assert it to be 42
   unseal r1 r1 r0
   mov r0 r5
   geta r4 r1
   mov r5 42
   mov r1 r3
   lea r1 1
   load r1 r1
   ;; ASSERT r1 r4 r5
   halt
fails:
   fail
data:
   ;; #(RO, b_lt, e_lt, b_lt); linking table_cap
   #0
   #(RWX, Global, main, main_end, data) ; writable cap of the main program
main_end:


adv_start:

    ; r1 contains the callback to main

    ; create the enclave capability
    mov r2 pc
    getb r3 pc
    add r3 r3 (enclave-adv_start)
    getb r4 pc
    add r4 r4 (enclave_end-adv_start)
    subseg r2 r3 r4
    lea r2 (enclave-adv_start+1)
    restrict r2 (RX, Global)
    mov r0 0

    ; restrict adv such that it doesn't intersect with the enclave
    getb r4 pc
    subseg pc r4 r3

    ; initialise the enclave
    einit r2
    ; store r1 in r31
    mov r31 r1

    ; prepare callback, and calls it to get the value
    mov r0 pc
    lea r0 4
    restrict r0 (E, Global)
    jmp r2

    ; callback
    ; < r1 contains unsealing cap >
    ; < r2 contains sealed cap >
    mov r0 r2
    jmp r31

enclave:
   #(RW, Global, data_enclave, data_enclave_end, data_enclave)

   ; get signing sealing key
   mov r1 pc
   lea r1 (-1)
   load r1 r1
   getb r2 r1
   geta r3 r1
   sub r2 r2 r3
   lea r1 r2
   load r1 r1
   gete r3 r1
   sub r2 r3 1
   subseg r1 r2 r3

   ; store the result (42) in a o-permission capability and sign it
   mov r2 pc
   geta r3 r2
   sub r3 42 r3
   lea r2 r3
   restrict r2 (O, Global)
   lea r1 1
   seal r2 r1 r2

   ; share the signed value and the unsealing key to the adversary
   restrict r1 (U, Global)
   jmp r0
enclave_end:

data_enclave:
    #0
    #0
    #0
    #0
    #0
    #0
data_enclave_end:

adv_end:
