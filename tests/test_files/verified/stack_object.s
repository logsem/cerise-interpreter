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
halt
