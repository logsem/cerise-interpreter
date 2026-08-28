;;; Source: "Le Temps des Cerises" (OOPSLA 2022), directed-capabilities case study.
;;; Rocq: https://github.com/logsem/cerise-stack-monotone/blob/master/theories/examples/stack_object.v
;;; Runnable companion to the verified directed-capability stack-object case study.
;;; The object state is initialized through a directed stack capability.
start:
storeU stk 0 3
storeU stk 0 21
storeU stk (-2) 22
loadU r0 stk -2
loadU r1 stk -1
mov r2 stk
promoteU r2
;;; Transfer to an example adversary continuation. It is a concrete example;
;;; the verified case study permits arbitrary code with the shared authority.
mov r30 pc
lea r30 (example_adversary - &CURRENT_ADDR + 1)
subseg r30 example_adversary example_adversary_end
restrict r30 (RX, Global)
jmp r30
example_adversary:
;;; Example adversary continuation after the stack object is initialized.
halt
example_adversary_end:
