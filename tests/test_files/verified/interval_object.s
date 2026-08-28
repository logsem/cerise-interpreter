;;; Runnable companion to Cerise's verified interval-object case study.
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
halt
interval:
# 0
# 0
# 0
interval_end:
