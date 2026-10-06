# sexp-edit

Paredit-style structural editing and indentation of Lisp text, in portable
Common Lisp with no dependencies.

Every command is a pure function from a string and a caret offset to a new
string and offset, or `NIL` when it declines (and the front end does what the
key would have done without it):

```lisp
(sexp-edit:delete-pair-backward "(setq foo 42)" 13)   ; => "(setq foo 42)", 12
(sexp-edit:insert-pair "(list " 6)                    ; => "(list ()", 7
(sexp-edit:newline-and-indent "(defun foo (n))" 14)   ; => "(defun foo (n)
                                                      ;      )", 17
```

It is shared by [Heml](https://github.com/lispnik/heml)'s Lisp mode and the Lisp
Listener's REPL and iOS editor, so that the same key does the same thing in
each. A front end hands over a line of input, or the top-level form around its
caret, and writes back what changed.

## What it does

- **Balanced insertion.** `(` inserts `()`, `"` a pair of quotes (escaped inside
  a string), and `)` steps over a closing paren that is already there.
- **Balanced deletion.** Backspace after a matched closing paren moves inside
  the list, after an opening one out of it, and over a string's quote likewise;
  an empty `()` or `""` goes whole; an unmatched paren may be deleted, since that
  is what fixes the text. Forward Delete is the mirror image.
- **Structure.** `forward-sexp`, `backward-sexp`, `kill-sexp`, `wrap-round`,
  `splice`, `slurp-forward`, `barf-forward`, `slurp-backward`, `barf-backward`,
  `raise-sexp`, `transpose-sexps`.
- **Indentation.** `indentation-at` and `newline-and-indent`: a body form two in
  once its distinguished arguments are written and those four in until then, as
  Emacs has it (which operators have them comes from a table of forms, from
  `DEF...`, or from a macro's own `&body`), a call under its first argument, a
  line under a previous element that begins its own line, `loop`'s clauses
  under the first, a function defined by `flet`, `labels` or `macrolet` like a
  `defun`, and data -- `'(...)` and `#(...)`, but not a backquoted template --
  one in. `defindent` adds to
  the table; `*lambda-list-function*` says where macros' lambda lists come from.

The scanner knows strings, `;` and nested `#| |#` comments, `|symbols|`,
`#\x` and named characters such as `#\Newline`, and escapes.

## Keys

`*paredit-keys*` is a default key table in Emacs notation (`"C-M-f"`, `"M-("`,
`"Backspace"`) that front ends may read; `(setf (paredit-key spec) command)`
rebinds and calls `*keys-changed-functions*`.

## Tests

`make test` (SBCL), `make test-ecl` (ECL). The edits are in `tests/cases.lisp`
as data, `*edit-cases*`, so that a front end can replay them through its own
glue.
