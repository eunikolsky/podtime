MAIN_TEST_TARGET = podtime:test:unit-test
INTEGRATION_TEST_TARGET = podtime:test:integration-test

.PHONY:
check: check-build check-build-int-tests check-test check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-build-int-tests:
	stack --verbosity error build --no-run-tests --fast $(INTEGRATION_TEST_TARGET)

.PHONY:
check-test:
	stack --verbosity error test --fast --ta='-f silent' $(MAIN_TEST_TARGET)

.PHONY:
check-hlint: check-hlint-other check-hlint-program

.PHONY:
check-hlint-program:
	hlint -j4 -h program/.hlint.yaml program

.PHONY:
check-hlint-other:
	hlint -j4 src test int-test

.PHONY:
testd:
	@ghcid --command "HSPEC_FORMAT=failed-examples stack ghci --test --main-is $(MAIN_TEST_TARGET) --ghci-options=-fobject-code" --test "main"

# use like this: `m testd-match MATCH=ConduitExtra`
.PHONY:
testd-match:
	@ghcid --command "stack ghci --test --main-is $(MAIN_TEST_TARGET) --ghci-options=-fobject-code" --test ":main --match \"$${MATCH}\""

.PHONY:
int-testd:
	@ghcid --command "stack ghci --test --main-is $(INTEGRATION_TEST_TARGET) --ghci-options=-fobject-code" --test ":main --fail-fast --rerun --rerun-all-on-success --failure-report=int-testd.report"

.PHONY:
testfw:
	@stack test --fast --file-watch $(MAIN_TEST_TARGET)

.PHONY:
int-testfw:
	@stack test --fast --file-watch $(INTEGRATION_TEST_TARGET)

# run like this: `m testfw-seed SEED=1032969830`
.PHONY:
testfw-seed:
	@stack test --fast --file-watch --ta="--seed $${SEED}" $(MAIN_TEST_TARGET)

.PHONY:
buildd:
	@ghcid --command "stack ghci"

.PHONY:
buildfw:
	@stack build --fast --file-watch
