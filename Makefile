TESTS_EL = $(wildcard test/*.el)
TESTS = $(TESTS_EL:test/%.el=%.el)
TESTS_LOG = $(TESTS:%.el=log/%.el)

$(warning $(TESTS_EL))

.PHONY: all test

all:

test: log $(TESTS_LOG)

log:
	mkdir -p $@

$(TESTS_LOG): log/%.el: test/%.el %.el
	emacs -batch -Q -L . -l $< -f ert-run-tests-batch-and-exit 2>&1 |tee $@
