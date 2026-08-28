;;; Source: "Le Temps des Cerises" (OOPSLA 2022), directed-capabilities case study.
;;; Rocq: https://github.com/logsem/cerise-stack-monotone/blob/master/theories/examples/downwards_lse.v
;;; Runnable companion to the verified downward-LSE case study.
;;; Directed stack authority initializes private state before being promoted.
start:
storeU stk 0 2
storeU stk 0 11
storeU stk (-2) 12
loadU r0 stk -2
loadU r1 stk -1
mov r2 stk
promoteU r2
;;; Transfer to an example adversary continuation. It is representative only:
;;; the verified theorem quantifies over arbitrary admissible adversary code.
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
example_adversary:
;;; Example adversary continuation after stack-frame initialization.
halt
example_adversary_end:
