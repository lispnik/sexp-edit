;;;; src/indent.lisp -- a newline, indented the way Lisp is indented.
;;;;
;;;; NEWLINE-AND-INDENT is a command of the same shape as the ones in
;;;; src/paredit.lisp -- (text offset) -> (values text offset) -- so it runs
;;;; through the same glue and make test covers it the same way: the Lisp
;;;; Listener's Option-Return (plain Return submits there), its iOS editor's
;;;; Return, and Heml's Return and C-j in Lisp mode.  INDENTATION-AT alone is
;;;; what an editor indents an existing line by.
;;;;
;;;; The rules are the Listener's and Heml's together: Heml's table of forms
;;;; and its two rules for what the table cannot say -- a function defined by
;;;; FLET, LABELS or MACROLET is indented like a DEFUN, and a line follows a
;;;; previous element that begins its own line, as whoever wrote it chose.
;;;;
;;;; Three rules, which between them are nearly everything typed at a listener:
;;;;
;;;;   (defun foo (n)          a BODY form: two past its paren, once its
;;;;     ...)                  distinguished arguments are all written
;;;;
;;;;   (list a                 an ordinary call with an argument on its first
;;;;         b)                line: under that argument
;;;;
;;;;   ((a b)                  anything else -- data, a list in operator
;;;;    (c d))                 position, a call with nothing after it: one past
;;;;
;;;; Which operators take a body is looked up in the live image: a macro whose
;;;; lambda list has &BODY says so itself, which covers the user's own macros
;;;; with no table to maintain.  The table below is for what cannot say --
;;;; special operators have no lambda list, and HANDLER-CASE and DEFMETHOD are
;;;; &REST where their shape is really a body -- and Emacs's own rule that
;;;; anything named DEF... is indented like DEFUN.
;;;;
;;;; Columns are columns on the screen: a tab goes to the next multiple of
;;;; *TAB-WIDTH*.  The text's first line may start further right than its
;;;; offset says -- after the Listener's prompt -- and *INDENT-FIRST-COLUMN* is
;;;; how far, which the front end binds.

(in-package #:sexp-edit)

(defvar *indent-first-column* 0
  "The transcript column the input region's first character sits in -- the
width of the prompt in front of it.")

(defvar *indent-package* nil
  "The package an operator is looked up in, to ask whether it takes a body.
NIL means *PACKAGE*.")

(defvar *tab-width* 8
  "How many columns a tab in the text advances to the next multiple of.")

(defparameter *indent-body-counts*
  '(;; Special operators, which have no lambda list to ask.
    ("BLOCK" . 1) ("CATCH" . 1) ("EVAL-WHEN" . 1) ("FLET" . 1) ("LABELS" . 1)
    ("MACROLET" . 1) ("SYMBOL-MACROLET" . 1) ("LET" . 1) ("LET*" . 1)
    ("LOCALLY" . 0) ("PROGN" . 0) ("PROGV" . 2) ("TAGBODY" . 0)
    ("UNWIND-PROTECT" . 1) ("MULTIPLE-VALUE-PROG1" . 1) ("MULTIPLE-VALUE-CALL" . 1)
    ("COMPILER-LET" . 1) ("LAMBDA" . 1)
    ;; Macros whose lambda list is &REST where their shape is a body, or whose
    ;; lambda list a Lisp may not keep.
    ("HANDLER-CASE" . 1) ("HANDLER-BIND" . 1) ("RESTART-CASE" . 1)
    ("RESTART-BIND" . 1) ("WITH-SIMPLE-RESTART" . 1)
    ("CASE" . 1) ("ECASE" . 1) ("CCASE" . 1)
    ("TYPECASE" . 1) ("ETYPECASE" . 1) ("CTYPECASE" . 1)
    ("DESTRUCTURING-BIND" . 2) ("MULTIPLE-VALUE-BIND" . 2) ("MULTIPLE-VALUE-SETQ" . 1)
    ("WITH-SLOTS" . 2) ("WITH-ACCESSORS" . 2) ("PRINT-UNREADABLE-OBJECT" . 1)
    ("WHEN" . 1) ("UNLESS" . 1) ("PROG1" . 1) ("DOLIST" . 1) ("DOTIMES" . 1)
    ("DO" . 2) ("DO*" . 2) ("DO-SYMBOLS" . 1) ("DO-EXTERNAL-SYMBOLS" . 1)
    ("DO-ALL-SYMBOLS" . 1) ("LOOP" . 0)
    ("WITH-OPEN-FILE" . 1) ("WITH-OPEN-STREAM" . 1) ("WITH-INPUT-FROM-STRING" . 1)
    ("WITH-OUTPUT-TO-STRING" . 1) ("WITH-PACKAGE-ITERATOR" . 1)
    ;; Definitions: what anything named DEF... gets unless it is here.
    ("DEFUN" . 2) ("DEFMACRO" . 2) ("DEFTYPE" . 2) ("DEFINE-COMPILER-MACRO" . 2)
    ("DEFINE-SETF-EXPANDER" . 2) ("DEFINE-CONDITION" . 2) ("DEFCLASS" . 2)
    ("DEFVAR" . 1) ("DEFPARAMETER" . 1) ("DEFCONSTANT" . 1) ("DEFPACKAGE" . 1)
    ("DEFSTRUCT" . 1))
  "Operator name -> how many distinguished arguments precede its body, for the
operators whose lambda list cannot say.  By NAME, as Emacs does it, so a
shadowing symbol of the same name indents the same way.  DEFINDENT adds to it.")

(defun defindent (name count)
  "Indent forms whose operator is NAME (a string or symbol) as having COUNT
distinguished arguments before a body; COUNT NIL takes NAME out of the table."
  (let ((name (string-upcase (string name))))
    (setf *indent-body-counts* (remove name *indent-body-counts* :key #'car :test #'string=))
    (when count
      (push (cons name count) *indent-body-counts*))
    name))

(defparameter *local-definers* '("FLET" "LABELS" "MACROLET")
  "Operators whose first argument is a list of definitions, each indented as a
DEFUN is: (flet ((name (args) body...)) ...).")

(defun default-lambda-list (symbol)
  "The lambda list of the macro SYMBOL names in this Lisp, or NIL."
  (ignore-errors
   #+sbcl (sb-kernel:%fun-lambda-list (macro-function symbol))
   #+ecl (ext:function-lambda-list symbol)
   #-(or sbcl ecl) nil))

(defvar *lambda-list-function* 'default-lambda-list
  "A function of a macro's symbol answering its lambda list, or NIL: asked
whether the macro has an &BODY, and where.  An editor whose code runs in
another Lisp may ask that one.")

;;; Where things are ---------------------------------------------------------------

(defun innermost-open-paren (text offset)
  "The offset of the innermost ( before OFFSET that is not closed before it,
or NIL at top level.  The second value is true when OFFSET is inside a string,
a |symbol| or a #| block comment |#, where a newline is part of the text and no
indentation belongs, and the third which (:STRING, :SYMBOL or :BLOCK-COMMENT)
and the fourth where it began.  A `;' comment is not one of them: the newline
ends it."
  (let* ((stack '()) (i 0) (len (length text)) (offset (min offset len)))
    (loop while (< i offset) do
      (multiple-value-bind (skip kind closed) (skip-non-code text i len)
        (cond ((null skip)
               (case (char text i)
                 (#\( (push i stack))
                 (#\) (pop stack)))
               (incf i))
              ((and (or (< offset skip) (and (= offset skip) (not closed)))
                    (member kind '(:string :symbol :block-comment)))
               (return-from innermost-open-paren (values (first stack) t kind i)))
              (t (setf i skip)))))
    (values (first stack) nil)))

(defun text-column (text position)
  "The screen column of POSITION in TEXT: tabs advance to the next multiple of
*TAB-WIDTH*, and the first line starts at *INDENT-FIRST-COLUMN*."
  (let* ((newline (position #\Newline text :end position :from-end t))
         (start (if newline (1+ newline) 0))
         (column (if newline 0 *indent-first-column*)))
    (loop for i from start below position
          do (if (char= (char text i) #\Tab)
                 (setf column (* *tab-width* (1+ (floor column *tab-width*))))
                 (incf column)))
    column))

(defun begins-line-p (text position)
  "True when only spaces and tabs come before POSITION on its line."
  (loop for i downfrom (1- position) to 0
        do (case (char text i)
             (#\Newline (return t))
             ((#\Space #\Tab))
             (t (return nil)))
        finally (return t)))

(defun previous-line-indentation (text offset)
  "The indentation of the last line before OFFSET's that is not blank, or 0."
  (let ((end (or (position #\Newline text :end (min offset (length text)) :from-end t) 0)))
    (loop
      (when (<= end 0) (return 0))
      (let* ((start (let ((nl (position #\Newline text :end end :from-end t)))
                      (if nl (1+ nl) 0)))
             (first (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text
                                     :start start :end end)))
        (when first (return (text-column text first)))
        (setf end (max 0 (1- start)))))))

(defun same-line-p (text a b)
  (not (find #\Newline text :start (min a b) :end (max a b))))

(defun list-elements (text open offset)
  "The (START . END) spans of the elements of the list opening at OPEN that
begin before OFFSET."
  (let ((spans '()) (i (1+ open)) (visible (subseq text 0 offset)))
    (loop (multiple-value-bind (start end) (sexp-span-at visible i)
            (if (and start (< start offset) (< start end))
                (progn (push (cons start end) spans) (setf i end))
                (return))))
    (nreverse spans)))

;;; Which operators take a body ----------------------------------------------------

(defun token-symbol (token)
  "The symbol TOKEN names, if it exists; never interns.  NIL for a keyword, a
number, or anything else that is not an operator name."
  (unless (or (zerop (length token)) (char= (char token 0) #\:)
              (digit-char-p (char token 0)))
    (let* ((colon (position #\: token))
           (package (if colon
                        (find-package (string-upcase (subseq token 0 colon)))
                        (or *indent-package* *package*)))
           (name (string-upcase (string-left-trim ":" (subseq token (or colon 0))))))
      (and package (find-symbol name package)))))

(defun lambda-list-body-count (lambda-list)
  "How many arguments precede &BODY in LAMBDA-LIST, or NIL if it has none."
  (let ((count 0) (list lambda-list))
    (loop
      (when (atom list) (return nil))
      (let ((item (pop list)))
        (cond ((eq item '&body) (return count))
              ;; Each takes the variable after it, which is no argument.
              ((member item '(&whole &environment)) (pop list))
              ((member item lambda-list-keywords))
              (t (incf count)))))))

(defun operator-body-count (token)
  "How many distinguished arguments the operator TOKEN takes before its body:
0 or more for a body form, :DEFINITION for anything named DEF... not in the
table, or NIL for an ordinary call.

A WITH-... macro whose lambda list does not say is taken to have one, as
nearly all of them do; ECL's WITH-OPEN-FILE is &REST, for one."
  (let* ((name (string-upcase (subseq token (1+ (or (position #\: token :from-end t)
                                                     -1)))))
         (known (assoc name *indent-body-counts* :test #'string=))
         (prefixp (lambda (prefix)
                    (and (> (length name) (length prefix))
                         (string= prefix name :end2 (length prefix))))))
    (cond (known (cdr known))
          ((funcall prefixp "DEF") :definition)
          ((let ((symbol (token-symbol token)))
             (and symbol (macro-function symbol)
                  (ignore-errors
                   (lambda-list-body-count (funcall *lambda-list-function* symbol))))))
          ((funcall prefixp "WITH-") 1))))

;;; The indentation ----------------------------------------------------------------

(defun local-definition-p (text open)
  "True when the list opening at OPEN is one of the definitions in the first
argument of a local definer: (flet ((name (args) body...)) ...)."
  (let ((bindings (innermost-open-paren text open)))
    (when bindings
      (let ((definer (innermost-open-paren text bindings)))
        (when definer
          (let* ((elements (list-elements text definer bindings))
                 (operator (first elements)))
            (and operator (null (rest elements))
                 (let ((name (string-upcase (subseq text (car operator) (cdr operator)))))
                   (member (subseq name (1+ (or (position #\: name :from-end t) -1)))
                           *local-definers* :test #'string=)))))))))

(defun indentation-at (text offset)
  "The column a new line begun at OFFSET in TEXT should start in.  To indent an
existing line, OFFSET is where the line starts."
  (multiple-value-bind (open quoted kind quoted-start) (innermost-open-paren text offset)
    (cond
      ;; In a string, a line goes one past its opening quote; in a |symbol|
      ;; or a block comment, it keeps the previous line's indentation.
      (quoted
       (if (eq kind :string)
           (1+ (text-column text quoted-start))
           (previous-line-indentation text offset)))
      ((null open) 0)
      (t
       (let* ((column (text-column text open))
              (elements (list-elements text open offset))
              (operator (first elements))
              (token (and operator (subseq text (car operator) (cdr operator))))
              (last (car (last elements))))
         (flet ((follow-or (otherwise)
                  ;; The last element before the line begins its own line:
                  ;; this one lines up with it, as its writer chose.
                  (if (and last (not (eq last operator)) (begins-line-p text (car last)))
                      (text-column text (car last))
                      otherwise)))
           (cond
             ;; A quoted list is data, whatever its first element looks like,
             ;; and so is a vector.  A backquoted one is a template of code,
             ;; and #'(lambda ...) is a function.
             ((quoted-list-p text open)
              (follow-or (1+ column)))
             ((or (null operator) (not (token-symbol-like-p token)))
              (follow-or (1+ column)))
             ;; A function FLET, LABELS or MACROLET defines: like a DEFUN.
             ((local-definition-p text open) (+ column 2))
             (t
              (let ((count (operator-body-count token))
                    (arguments (rest elements)))
                (cond
                  ((eq count :definition) (+ column 2))
                  ((and count (>= (length arguments) count)) (+ column 2))
                  ;; Still among a body form's distinguished arguments, or an
                  ;; ordinary call: under the previous argument if that
                  ;; begins its line, else under the first if it is on the
                  ;; operator's line.
                  ((and arguments (same-line-p text (car operator) (car (first arguments))))
                   (follow-or (text-column text (car (first arguments)))))
                  (count (follow-or (+ column 4)))
                  (t (follow-or (1+ column)))))))))))))

(defun quoted-list-p (text open)
  "True when the list opening at OPEN is written as data: '(...) or #(...)."
  (and (plusp open)
       (case (char text (1- open))
         (#\' (not (and (> open 1) (char= (char text (- open 2)) #\#))))
         (#\# t))))

(defun token-symbol-like-p (token)
  "True when TOKEN could name an operator: not a list, a string, a keyword or a
number."
  (and (plusp (length token))
       (not (find (char token 0) "(\"':#`,"))
       (not (digit-char-p (char token 0)))))

(defun newline-and-indent (text offset)
  "Break the line at OFFSET and indent the new one.  Never declines.

Spaces either side of the break go: trailing ones would be left dangling on the
line above, and leading ones would push the rest of the line past the column
this chose.  Inside a string nothing is touched but the newline itself."
  (multiple-value-bind (open in-string) (innermost-open-paren text offset)
    (declare (ignore open))
    (if in-string
        (values (concatenate 'string (subseq text 0 offset) (string #\Newline)
                             (subseq text offset))
                (1+ offset))
        (let* ((before (string-right-trim '(#\Space #\Tab) (subseq text 0 offset)))
               (after (string-left-trim '(#\Space #\Tab) (subseq text offset)))
               (indent (if *auto-indent-enabled*
                           (indentation-at before (length before))
                           0))
               (head (concatenate 'string before (string #\Newline)
                                  (make-string indent :initial-element #\Space))))
          (values (concatenate 'string head after) (length head))))))
