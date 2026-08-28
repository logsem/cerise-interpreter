;;; Source: "Efficient and Provable Local Capability Revocation using
;;; Uninitialized Capabilities" (POPL 2021), Section 6, "Awkward Example".
;;; Rocq: https://github.com/logsem/cerise-stack/blob/main/theories/examples/awkward_example.v
;;; Runnable companion to the verified awkward example with revocation.
;;; An uninitialized stack capability is consumed while initializing a frame.
start:
storeU stk 0 1
storeU stk 0 43
storeU stk (-2) 42
loadU r0 stk -2
loadU r1 stk -1
mov r2 stk
promoteU r2
;;; Transfer to an example adversary continuation. In the verified case study,
;;; this continuation may be arbitrary code with only the shared authority.
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
example_adversary:
;;; Example adversary continuation after revocation.
halt
example_adversary_end:
