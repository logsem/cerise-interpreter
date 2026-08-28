;;; Source: Cerise journal paper (JACM 2024), Section 8.2, "Dynamic Sealing".
;;; Rocq: theories/examples/dynamic_sealing.v (make_seal_preamble_instrs).
;;; This companion consumes a fresh singleton sealing range before transferring
;;; the sealed value to the example adversary; it transfers no unsealer.
start:
	;;; Derive the private payload capability while the initial PC still spans it.
	mov r6 pc
	lea r6 (private_value - &CURRENT_ADDR + 1)
	subseg r6 private_value private_value_end
	restrict r6 (RW, Global)
	mov r0 pc
	lea r0 (after_make_seal - &CURRENT_ADDR + 1)
	subseg r0 after_make_seal after_make_seal_end
	restrict r0 (E, Global)
	mov r30 pc
	lea r30 (make_seal - &CURRENT_ADDR + 1)
	subseg r30 make_seal dynamic_sealing_end
	restrict r30 (E, Global)
	jmp r30
after_make_seal:
	seal r1 r2 r6
;;; The verified logical relation permits arbitrary code here. This concrete
;;; adversary gets r1 only, and cannot derive a matching unsealing capability.
	mov r2 0
	mov r6 0
	mov r30 pc
	lea r30 (example_adversary - &CURRENT_ADDR + 1)
	subseg r30 example_adversary example_adversary_end
	restrict r30 (RX, Global)
	jmp r30
after_make_seal_end:
private_value:
# 0
private_value_end:
example_adversary:
	getwtype r4 r1
	halt
example_adversary_end:
;;; The fresh-range allocator advances a private cursor and returns precisely
;;; one otype in r2, corresponding to make_seal's fresh authority.
make_seal:
	mov r8 pc
	lea r8 (seal_cursor_cap - &CURRENT_ADDR + 1)
	load r8 r8
	load r2 r8
	lea r2 1
	store r8 r2
	lea r2 -1
	jmp r0
seal_cursor:
#[SU, Global, 0, 10, 0]
seal_cursor_end:
seal_cursor_cap:
#(RW, Global, seal_cursor, seal_cursor_end, seal_cursor)
dynamic_sealing_end:
