.PHONY:
check: check-hlint

.PHONY:
check-hlint:
	hlint src
