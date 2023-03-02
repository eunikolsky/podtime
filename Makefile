.PHONY:
check: check-build check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-hlint:
	hlint -j4 src
