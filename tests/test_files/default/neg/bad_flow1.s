	mov r1 pc
    seal r1 r0 r1
	unseal r1 r0 r1             ; FAIL: attempt to unseal a sentry
	halt
