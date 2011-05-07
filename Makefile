#!/usr/bin/make

EMACS    = emacs --batch --quick --directory .
WGET     = wget --timestamping
cached   = ert.el
clean   += $(cached)
testlogs = test-perform-replace-with-dict-el.log \
           test-shell-command-string-el.log \
           test-copyedit-ja-el.log
mostlyclean += $(testlogs)

.PHONY: all test mostlyclean clean

all: test

test: $(testlogs)

test-%-el.log: test/test-%.el ert.el
	$(EMACS) --eval '(load-file "$<")' 2>&1 | tee $@

ert.el:
	$(WGET) --output-document=$@ \
	http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el

mostlyclean:
	-rm -f $(mostlyclean)

clean: mostlyclean
	-rm -f $(clean)
