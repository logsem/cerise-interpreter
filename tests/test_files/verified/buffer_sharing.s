;;; Source: Cerise journal paper (JACM 2024), Sections 2.3 and 6.1,
;;; "sharing a sub-buffer with unknown code".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/buffer.v
;;; Runnable execution of buffer_code from the verified case study. The source
;;; performs a one-way jump to arbitrary code after placing the sub-buffer in r1.
start:
;;; r0 plays the paper's arbitrary adversary entry capability.
mov r0 pc
lea r0 (example_adversary - &CURRENT_ADDR + 1)
subseg r0 example_adversary example_adversary_end
restrict r0 (RX, Global)
;;; This is buffer_code: derive a capability for exactly the public three words
;;; and then transfer control to the adversary in r0.
mov r1 pc
lea r1 (buffer - &CURRENT_ADDR + 1)
subseg r1 buffer buffer_public_end
jmp r0
buffer:
# 72
# 105
# 0
buffer_public_end:
;;; The secret is adjacent in memory but outside r1's bounds.
# 42
example_adversary:
;;; Example adversary only. It uses r1, the capability received from
;;; buffer_code, and cannot reach the following secret word.
load r2 r1
lea r1 1
load r2 r1
halt
example_adversary_end:
