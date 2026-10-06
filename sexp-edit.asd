;;;; sexp-edit.asd -- structural editing of Lisp text, as pure functions.

(asdf:defsystem "sexp-edit"
  :description "Paredit-style structural editing and indentation of Lisp text:
pure functions from a string and a caret offset to a new string and offset,
for any front end -- an editor's buffer, a REPL's input line, a text view."
  :author "Matthew Kennedy"
  :license "MIT"
  :version "0.1.0"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "sexp")
                             (:file "paredit")
                             (:file "keymap")
                             (:file "indent"))))
  :in-order-to ((test-op (test-op "sexp-edit/test"))))

(asdf:defsystem "sexp-edit/test"
  :description "The tests, and the corpus of edits front ends replay."
  :depends-on ("sexp-edit")
  :components ((:module "tests"
                :serial t
                :components ((:file "cases")
                             (:file "tests"))))
  :perform (test-op (o c)
             (unless (uiop:symbol-call :sexp-edit-tests '#:run)
               (error "sexp-edit tests failed"))))
