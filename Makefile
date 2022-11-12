#!/usr/bin/make

EMACS    = emacs --batch --no-init-file --no-site-file --no-splash --directory . \
           --eval="(progn (package-initialize) (prefer-coding-system 'utf-8-unix))"
           # (package-initialize): make elpa/melpa packages (color-moccur) findable
           # (prefer-coding-system 'utf-8-unix): make non-ASCII file (copyedit-ja.el) loadable

all: check

check: test/test-*.el
	for f in $^; do \
	  $(EMACS) --load="$$f" 2>&1 \
	  | tee `basename --suffix=.el $$f`.log; \
	done

clean:
	-rm -f *.log

.PHONY: all check clean
