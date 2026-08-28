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
halt
