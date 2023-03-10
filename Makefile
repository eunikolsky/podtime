.PHONY:
check: check-build check-test check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-test:
	stack --verbosity error test --fast --ta='-f silent'

.PHONY:
check-hlint:
	hlint -j4 src program test

.PHONY:
testd:
	@ghcid --command "stack ghci --test --main-is $$( stack ide targets 2>&1 | grep -F :test: ) --ghci-options=-fobject-code" --test "main"

.PHONY:
testfw:
	@stack test --fast --file-watch

# run like this: `m testfw-seed SEED=1032969830`
.PHONY:
testfw-seed:
	@stack test --fast --file-watch --ta="--seed $${SEED}"
