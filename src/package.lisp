;;;; src/package.lisp

(defpackage #:sexp-edit
  (:use #:cl)
  (:documentation "Structural editing of Lisp text.  Every command takes a
string and a caret offset into it and answers (values NEW-TEXT NEW-OFFSET), or
NIL when it declines, so that a front end does what the key would have done
without it.  Shared by Heml's Lisp mode, the Lisp Listener's REPL, and its iOS
editor, so that the same key does the same thing in each.")
  (:export
   ;; Scanning.
   #:skip-non-code #:paren-match-offset #:code-position-p #:in-string-p #:token-at
   #:enclosing-list #:sexp-bounds #:inner-list #:sexp-span-at #:sexp-spans
   #:parent-siblings #:apply-structural-edit
   ;; Commands.
   #:insert-pair #:insert-quote #:close-or-skip
   #:delete-pair-backward #:delete-pair-forward #:delete-paren-p
   #:forward-sexp #:backward-sexp
   #:kill-sexp #:wrap-round #:splice #:slurp-forward #:barf-forward
   #:slurp-backward #:barf-backward #:raise-sexp #:transpose-sexps
   #:*paredit-commands* #:run-paredit-command #:*command-error-function*
   ;; Keys.
   #:*paredit-enabled* #:*paredit-keys* #:paredit-key #:*keys-changed-functions*
   #:parse-key-spec #:paredit-self-insert-key-p #:paredit-command-for
   ;; Indentation.
   #:*auto-indent-enabled* #:*indent-first-column* #:*indent-package*
   #:*tab-width* #:*indent-body-counts* #:*local-definers* #:defindent
   #:*lambda-list-function* #:innermost-open-paren #:text-column
   #:list-elements #:token-symbol #:operator-body-count #:indentation-at
   #:newline-and-indent))
