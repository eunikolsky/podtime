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

.PHONY:
testd:
	@ghcid --command "stack ghci --test --main-is $$( stack ide targets 2>&1 | grep -F :test: ) --ghci-options=-fobject-code" --test "main"
