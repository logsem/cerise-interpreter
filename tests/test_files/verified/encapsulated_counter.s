;;; Runnable companion to Cerise's verified encapsulated-counter case study.
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
halt
counter:
# 0
counter_end:
