	mov r1 cgp
	;; Cannot restrict Local to Global
	restrict r1 ([R W LG LM], LOCAL)
	restrict r1 ([R W LG LM], GLOBAL)
	halt
