;;; Source: Cerise journal paper (JACM 2024), Section 2.4, "a counter compartment".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/counter.v
;;; Runnable companion to the verified encapsulated-counter case study.
;;; The counter's mutable state is reachable only through a local capability.
start:
mov r0 pc
lea r0 (counter - start)
subseg r0 counter counter_end
restrict r0 (RW, Local)
store r0 0
load r1 r0
add r1 r1 1
store r0 r1
load r2 r0
;;; Transfer to an example adversary continuation. The verified theorem allows
;;; arbitrary adversary code; this concrete adversary receives no local state.
mov r0 0
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
counter:
# 0
counter_end:
example_adversary:
;;; Example adversary: it can observe r2, but not the counter capability.
halt
example_adversary_end:
