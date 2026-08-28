;;; Source: Cerise journal paper (JACM 2024), Sections 2.4 and 6.2.
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/lse.v
;;; Runnable companion to the verified local-state-encapsulation case study.
;;; A local capability is used to update private component state.
start:
mov r0 pc
lea r0 (private_state - start)
subseg r0 private_state private_state_end
restrict r0 (RW, Local)
store r0 41
load r1 r0
add r1 r1 1
store r0 r1
load r2 r0
;;; Transfer to an example adversary continuation. The verified theorem allows
;;; arbitrary adversary code; it is not given the local private-state capability.
mov r0 0
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
private_state:
# 0
private_state_end:
example_adversary:
;;; Example adversary continuation.
halt
example_adversary_end:
