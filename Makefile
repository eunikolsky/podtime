.PHONY:
check: check-build check-test check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-test:
	stack --verbosity error test --fast

.PHONY:
check-hlint:
	hlint -j4 src program test
