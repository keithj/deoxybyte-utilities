
# This Makefile is, for the most part, a wrapper around the Lisp build
# system ASDF. It provides targets for building non-Lisp components,
# such as texinfo documentation.


.PHONY:	all fasl doc clean

default: all

all: fasl doc

fasl:
	sbcl --noinform --noprint \
	--eval "(progn (asdf:operate 'asdf:compile-op :cl-gp-utilities) (quit))"

doc:
	sbcl --noinform --noprint --load make-doc.lisp

test:
	sbcl --noinform --noprint \
	--eval "(progn (asdf:operate 'asdf:test-op :cl-gp-utilities) (quit))"

clean:
	find . -name \*.fasl -exec rm {} \;
