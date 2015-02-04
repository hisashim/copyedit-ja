#!/usr/bin/make

EMACS    = emacs --batch --quick --directory .
WGET     = wget --timestamping
SRC      = perform-replace-with-dict.el \
           shell-command-string.el \
           copyedit-ja.el
xyzzy_l  = $(patsubst %.el,%.l,$(SRC))
cached   = ert.el
clean   += $(cached)
testlogs = $(foreach f,$(SRC),$(f:%.el=test-%-el.log))

mostlyclean += $(testlogs) $(xyzzy_l)

all: test

%.l: %.el
	cp $< $@

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

.PHONY: all test mostlyclean clean
