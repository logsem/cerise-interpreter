;;; Source: Cerise journal paper (JACM 2024), Section 7.5,
;;; "read-only sharing of dynamically allocated memory".
;;; Rocq: https://github.com/logsem/cerise/blob/journal/theories/examples/ocpl_lowval_like.v
;;; Runnable companion to the journal paper's verified read-only-sharing example.
;;; The public capability is restricted to RO after private initialization.
start:
mov r0 pc
lea r0 (cell - start)
subseg r0 cell cell_end
restrict r0 (RW, Global)
store r0 1
mov r1 r0
restrict r1 (RO, Global)
;;; Transfer to an example adversary. The verified theorem permits arbitrary
;;; adversary code; it receives only the read-only capability in r1.
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
cell:
# 0
cell_end:
example_adversary:
;;; Example adversary: it can read the cell but has no write authority.
load r2 r1
halt
example_adversary_end:
