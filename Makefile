#!/usr/bin/make

EMACS    = emacs --batch --quick --directory . --eval "(prefer-coding-system 'utf-8-unix)"
SRC      = perform-replace-with-dict.el \
           shell-command-string.el \
           copyedit-ja.el
testlogs = $(foreach f,$(SRC),$(f:%.el=test-%-el.log))

test-%-el.log: test/test-%.el
	$(EMACS) --eval '(load-file "$<")' 2>&1 | tee $@

all: test

test: $(testlogs)

clean:
	-rm -f $(testlogs)

.PHONY: all test clean
