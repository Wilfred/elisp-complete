# elisp-complete

A smart contextual completion package for writing Emacs Lisp.

## Features

Completion of library names inside `(require)` and `(provide)` forms.

Complete `&optional` etc in argument lists.

## Demo

```
(setq-local company-backends '(elisp-complete))
(setq-local company-minimum-prefix-length 1)

;; Now, run M-x elisp-complete in the current buffer.
```
