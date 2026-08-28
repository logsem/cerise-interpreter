;;; Source: Cerise journal paper (JACM 2024), Section 8.2, "Dynamic Sealing".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/dynamic_sealing.v
;;; Runnable companion to the journal paper's verified dynamic-sealing example.
;;; A seal range hides a private cell until it is unsealed by its owner.
start:
mov r0 pc
lea r0 (private_value - start)
subseg r0 private_value private_value_end
restrict r0 (RW, Global)
mov r2 pc
lea r2 (seal_range - &CURRENT_ADDR + 1)
load r2 r2
seal r1 r2 r0
;;; Transfer to an example adversary. The verified theorem permits arbitrary
;;; adversary code; it receives the sealed value but not the unsealing range.
mov r2 0
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
seal_range:
#[SU, Global, 0, 10, 0]
private_value:
# 0
private_value_end:
example_adversary:
;;; Example adversary: it receives the sealed object but cannot unseal r1.
;;; It leaves r4 at 0 to record that no private payload was recovered.
mov r4 0
halt
example_adversary_end:
