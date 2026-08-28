;;; Runnable companion to Cerise's verified local-state-encapsulation case study.
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
halt
private_state:
# 0
private_state_end:
