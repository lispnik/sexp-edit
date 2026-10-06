;;;; tests/cases.lisp -- the corpus of edits, as data.
;;;;
;;;; Each case is (COMMAND BEFORE AFTER LABEL): COMMAND run on BEFORE, where |
;;;; marks the caret, answers AFTER, with its caret, or :DECLINED.  / in a
;;;; string stands for a newline.  Data rather than code so that a front end --
;;;; Heml's buffers, the Lisp Listener's text views -- can replay every case
;;;; through its own glue and show that it does what the library does.

(defpackage #:sexp-edit-tests
  (:use #:cl #:sexp-edit)
  (:export #:*edit-cases* #:run #:case-text #:case-offset #:render))

(in-package #:sexp-edit-tests)

(defparameter *edit-cases*
  '(;; Balanced insertion.
    (insert-pair "(list |" "(list (|)" "( inserts a pair")
    (insert-pair "(list \"a|\"" :declined "( inside a string is just a paren")
    (insert-pair "(list ; a|" :declined "and so in a comment")
    (insert-quote "(list |" "(list \"|\"" "\" inserts a pair")
    (insert-quote "(list \"hi|\")" "(list \"hi\"|)" "\" before the closing quote steps over it")
    (insert-quote "(list \"|\")" "(list \"\"|)" "and so from an empty string")
    (insert-quote "(list \"a|b\")" "(list \"a\\\"|b\")" "in the middle of a string it goes in escaped")
    (close-or-skip "(list (a|)" "(list (a)|" ") steps over the close paren")
    (close-or-skip "(list (a|" :declined ") with nothing to step over declines")
    ;; Backspace.
    (delete-pair-backward "(list (|)" "(list |" "Backspace takes an empty pair whole")
    (delete-pair-backward "(list \"|\"" "(list |" "and an empty string whole")
    (delete-pair-backward "(setq foo 42)|" "(setq foo 42|)"
     "Backspace after a matched close paren moves inside the list")
    (delete-pair-backward "(list (a)|" "(list (a|)" "and so for an inner one")
    (delete-pair-backward "(list (|a)" "(list |(a)" "after a matched open paren it moves out")
    (delete-pair-backward "(list \"ab\"|)" "(list \"ab|\")" "after a string's closing quote it moves in")
    (delete-pair-backward "(list \"|ab\")" "(list |\"ab\")" "and after its opening quote, out")
    (delete-pair-backward "(list \"a\\\"|b\")" :declined "an escaped quote is an ordinary character")
    (delete-pair-backward "(list (a|" :declined "an UNMATCHED open paren may be deleted")
    (delete-pair-backward "list a)|" :declined "and an unmatched close paren")
    (delete-pair-backward "(list a|" :declined "and an ordinary character")
    (delete-pair-backward "(list \"(|\")" :declined "a paren in a string is a character")
    (delete-pair-backward "(list #\\)|)" :declined "and so is a character literal")
    ;; Forward Delete.
    (delete-pair-forward "(list |()" "(list |" "Delete takes an empty pair whole")
    (delete-pair-forward "(list |(a))" "(list (|a))" "before a matched open paren it moves in")
    (delete-pair-forward "(list (a|))" "(list (a)|)" "before a matched close paren, out")
    (delete-pair-forward "(list |\"ab\")" "(list \"|ab\")" "before a string's opening quote, in")
    (delete-pair-forward "(list |(a" :declined "an unmatched paren may be deleted")
    (delete-pair-forward "(list |a)" :declined "and an ordinary character")
    (delete-pair-forward "(list a|" :declined "and at the end, nothing")
    ;; Motion.
    (forward-sexp "|(a b) c" "(a b)| c" "forward-sexp steps over a form")
    (forward-sexp "(a |#\\Space b)" "(a #\\Space| b)" "a named character is one atom")
    (backward-sexp "(a b) c|" "(a b) |c" "backward-sexp steps back over one")
    ;; Structure.
    (kill-sexp "(list |(a b) c)" "(list |c)" "kill-sexp takes the form at the caret")
    (wrap-round "(list| a)" "(|(list a))" "wrap-round adds a pair around the form")
    (splice "(list (a| b))" "(list a| b)" "splice removes the parens around it")
    (slurp-forward "(list (a|) b)" "(list (a| b))" "slurp pulls the next form in")
    (barf-forward "(list (a b|))" "(list (a) |b)" "barf pushes the last form out")
    (slurp-backward "(list a (b|))" "(list (|a b))" "slurp-backward pulls the previous form in")
    (barf-backward "(list (a b|))" "(list |a (b))" "barf-backward pushes the first form out")
    (raise-sexp "(list (a|))" "|(a)" "raise-sexp replaces the enclosing form")
    (transpose-sexps "(list |a b)" "(list b |a)" "transpose swaps two siblings")
    ;; A newline, indented.
    (newline-and-indent "(defun foo (n)|)" "(defun foo (n)/  |)" "a DEFUN's body is two in")
    (newline-and-indent "(defun foo (n)/  (lambda ()|))" "(defun foo (n)/  (lambda ()/    |))"
     "and so is a LAMBDA's, inside it")
    (newline-and-indent "(list a|)" "(list a/      |)" "a call lines up under its first argument")
    (newline-and-indent "(foo :key 1|)" "(foo :key 1/     |)" "an unknown function is a call")
    (newline-and-indent "((a b)|)" "((a b)/ |)" "a list in operator position is one in")
    (newline-and-indent "'(a b|)" "'(a b/  |)" "and so is a quoted list")
    (newline-and-indent "`(let ((x 1))|)" "`(let ((x 1))/   |)" "a backquoted list is code")
    (newline-and-indent "#(a b|)" "#(a b/  |)" "a vector is data")
    (newline-and-indent "#'(lambda (x)|)" "#'(lambda (x)/    |)" "and #'(lambda ...) is a function")
    (newline-and-indent "(let ((x 1))|)" "(let ((x 1))/  |)" "LET, which has no lambda list to ask")
    (newline-and-indent "(destructuring-bind (a b)|)" "(destructuring-bind (a b)/                    |)"
     "a distinguished argument not yet written lines up with the first")
    (newline-and-indent "(unwind-protect|)" "(unwind-protect/    |)"
     "and with none on the line, it is four in")
    (newline-and-indent "(defmethod foo :around ((x t))|)" "(defmethod foo :around ((x t))/  |)"
     "anything named DEF... is indented like DEFUN")
    (newline-and-indent "(with-anything (x)|)" "(with-anything (x)/  |)"
     "a WITH-... nobody has defined is taken to have one argument")
    (newline-and-indent "(list \"(\" a|)" "(list \"(\" a/      |)" "a paren in a string is not a paren")
    (newline-and-indent "(list a |  b)" "(list a/      |b)" "spaces either side of the break go")
    (newline-and-indent "(format t \"a|b\")" "(format t \"a/|b\")" "inside a string, only the newline")
    (newline-and-indent "(a) |" "(a)/|" "at top level, column 0")
    ;; Heml's rules, now everyone's.
    (newline-and-indent "(flet ((twice (x)|)))" "(flet ((twice (x)/         |)))"
     "a function FLET defines is indented like a DEFUN")
    (newline-and-indent "(labels ((f ()/           (g))|)" "(labels ((f ()/           (g))/         |)"
     "and the next definition lines up with the first")
    (newline-and-indent "(loop for x in xs|)" "(loop for x in xs/  |)" "LOOP's clauses are two in")
    (newline-and-indent "(list a/      b|)" "(list a/      b/      |)" "a line follows the previous element")
    (newline-and-indent "(list/   a|)" "(list/   a/   |)"
     "even where whoever wrote it put it, not under the first argument")
    (newline-and-indent "(when (ready)|)" "(when (ready)/  |)" "WHEN's body is two in")))

(defun case-text (string)
  "STRING with its caret mark removed and / made a newline."
  (substitute #\Newline #\/ (remove #\| string)))

(defun case-offset (string)
  "Where the caret is in STRING, counted in the text CASE-TEXT answers."
  (position #\| string))

(defun render (text offset)
  "TEXT with | at OFFSET and its newlines written as /: a case's notation."
  (substitute #\/ #\Newline
              (concatenate 'string (subseq text 0 offset) "|" (subseq text offset))))
