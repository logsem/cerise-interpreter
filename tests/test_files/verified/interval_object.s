;;; Source: Cerise journal paper (JACM 2024), Section 8.1, "Interval Library and Client".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/interval_client.v
;;; Runnable companion to the verified interval-object case study.
;;; A bounded capability exposes exactly a three-word private interval.
start:
mov r0 pc
lea r0 (interval - start)
subseg r0 interval interval_end
restrict r0 (RW, Global)
lea r0 1
store r0 42
load r1 r0
getb r2 r0
gete r3 r0
sub r4 r3 r2
;;; Transfer to an example adversary continuation. The proof permits arbitrary
;;; adversary code, which only receives the bounded interval capability in r0.
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
interval:
# 0
# 0
# 0
interval_end:
example_adversary:
;;; Example adversary: it may read the bounded interval, then terminates.
load r5 r0
halt
example_adversary_end:
