;;; Source: Cerise journal paper (JACM 2024), Sections 2.3 and 6.1,
;;; "sharing a sub-buffer with unknown code".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/buffer.v
;;; Runnable companion to the journal paper's verified buffer-sharing example.
;;; The shared capability is bounded before the secret word at buffer_end.
start:
mov r0 pc
lea r0 (buffer - start)
subseg r0 buffer buffer_end
restrict r0 (RW, Global)
;;; Transfer to an example adversary. The proof allows arbitrary code, but it
;;; receives only r0, whose bounds exclude the secret word at buffer_end.
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
buffer:
# 72
# 105
buffer_end:
# 42
example_adversary:
;;; Example adversary: reads the shared buffer and cannot reach the secret.
load r1 r0
lea r0 1
load r2 r0
halt
example_adversary_end:
