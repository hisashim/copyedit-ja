#!/usr/bin/make

EMACS    = emacs --batch --no-init-file --no-site-file --no-splash --directory . \
           --eval="(progn (package-initialize) (prefer-coding-system 'utf-8-unix))"
           # (package-initialize): make elpa/melpa packages (color-moccur) findable
           # (prefer-coding-system 'utf-8-unix): make non-ASCII file (copyedit-ja.el) loadable

SRC      = copyedit-ja.el

testlogs = $(foreach f,$(SRC),$(f:%.el=test-%-el.log))

test-%-el.log: test/test-%.el
	$(EMACS) --load="$<" 2>&1 | tee $@

all: test

test: $(testlogs)

clean:
	-rm -f $(testlogs)

.PHONY: all test clean
