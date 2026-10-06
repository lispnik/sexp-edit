# sexp-edit: make test (SBCL), make test-ecl (ECL), make check (both).

SBCL ?= sbcl
ECL ?= ecl

.PHONY: test test-ecl check

test:
	$(SBCL) --noinform --non-interactive \
	  --eval '(push (truename "./") asdf:*central-registry*)' \
	  --eval '(asdf:load-system :sexp-edit/test)' \
	  --eval '(uiop:quit (if (uiop:symbol-call :sexp-edit-tests :run) 0 1))'

test-ecl:
	$(ECL) --eval '(require :asdf)' \
	  --eval '(push (truename "./") asdf:*central-registry*)' \
	  --eval '(asdf:load-system :sexp-edit/test)' \
	  --eval '(ext:quit (if (uiop:symbol-call :sexp-edit-tests :run) 0 1))'

check: test test-ecl
