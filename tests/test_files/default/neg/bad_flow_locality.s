	mov r1 cgp
	;; Cannot restrict Local to Global
	restrict r1 ([R W], LOCAL)
	restrict r1 ([R W], GLOBAL)
	halt
