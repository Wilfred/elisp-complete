;;; elisp-complete.el --- context-aware completion for emacs lisp buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes
;; Version: 0.1

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Complete symbols in emacs lisp buffers, considering context and
;; history to provide more relevant results.

;;; Ideas:

;; (defcustom racer-complete-in-comments
;;   nil
;;   "foo"
;;   :type 'boolean
;;   :group 'racer)
;;
;; Could complete group value, simple type values, and keys to
;; defcustom.

;; Auto insert space when completing (foo ) if foo takes multiple
;; args.
;;
;; Completion inside emacs style `backticks' inside docstrings and
;; comments.
;;
;; Even when we can't offer an exact set of options, offer the current
;; package as a prefix.
;;
;; Consider only offering autoloaded/required functions in the current
;; file.
;;
;; Offer sensible completion inside (declare ...) forms and
;; (interactive ...) forms.
;; 
;; Consider expanding macros to work out what's bound.
;;
;; Offer &rest and &optional in parameter lists.
;;
;; Offer bound variables inside let/condition-case.
;;
;; (require ...) and (provide ...) should be trivially completable.
;;
;; define-key and global-set-key: offer keymaps and interactive
;; commands.
;;
;; Autocomplete keywords inside rx.
;;
;; Show num args (dropdown) and docstring (minibuffer) when completing.
;;
;; Sort prefix first, but allow substring, so we offer
;; `map-char-table' for input "char-table".
;;
;; Write a crude type inference (look at integer and list assignments)
;; and prioritise arguments to e.g. `nth' (based on its docstring
;; saying arg type) that are known to be integer/lists.
;;
;; Don't offer completion inside quoted forms, except `(foo,
;; bar). This again requires macro expansion.
;;
;; Don't offer completion for the first argument of var-val pairs in
;; `let', as they're usually new vars.
;;
;; Offer completion of FOO in a docstring when `foo' is an argument to
;; a function.
;;
;; Offer completion of known signals when defining handlers in
;; `condition-case'.
;;
;; Offer completion of literals for argument of a known type,
;; particularly booleans. Types may be inferred from parameter names
;; in docstrings.
;;
;; Rank functions/variables more highly if they've been used in a
;; recent function body. See page 22 of
;; https://users.dcc.uchile.cl/~rrobbes/p/JASE-completion.pdf which
;; measures the effectiveness of this approach. This would require
;; advising `eval-last-sexp', `edebug-eval-defun', and `eval-buffer'.
;;
;; Also see if a user has recently viewed docs for a symbol.
;;
;; `defadvice' should offer completion for existing functions as the
;; first argument.
;;
;; Sort results with prefix matches first, then substrings (maybe
;; treating suffix matches more highly too).
;;
;; Fuzzy completion on hyphens, so `foo-bar' matches `foo--bar'.
;;
;; Complete #' for functions, and only functions.

;;; Code:

(require 'dash)
(require 's)
(require 'elisp-def)

(defvar elisp-complete--recent-syms nil)
(defvar elisp-complete--history-size 1000)

(defun elisp-complete--all-callable-syms (prefix)
  "Return a list of all bound syms that start with PREFIX."
  (let (syms)
    (mapatoms
     (lambda (sym)
       (when (and
              (fboundp sym)
              (s-prefix-p prefix (symbol-name sym)))
         (push sym syms))))
    syms))

(defun elisp-complete--global-syms (form)
  "Return all the symbol in FORM that are globally bound."
  ;; TODO: we can't macro expand, because we want to preserve macro
  ;; references. However, this erroneously offers binding variable
  ;; names (probably harmless) and function (due to #'foo, which will
  ;; lead to noise).
  (let* ((atoms (-flatten form))
         (syms (-filter #'symbolp atoms))
         ;; Ignore quote and function, because those probably result
         ;; from forms using 'foo and #'foo so it's unlikely they've
         ;; been used by the user.
         (syms (--filter (not (memq it '(quote function))) syms))
         (bound-syms (--filter (or (boundp it) (fboundp it)) syms)))
    (-uniq bound-syms)))

(defun elisp-complete--add-to-recent (form)
  (let* ((used-syms (elisp-complete--global-syms form))
         (syms (-uniq (append used-syms elisp-complete--recent-syms))))
    (setq
     elisp-complete--recent-syms
     (-take elisp-complete--history-size syms))))

(defun elisp-complete--read-top-level ()
  "Read the top-level form enclosing point."
  ;; We can't use `edebug-read-top-level-form' as it mutates some
  ;; edebug state and `C-u M-x edebug-eval-defun' stops working.
  (save-excursion
    (beginning-of-defun)
    (read (current-buffer))))

(defun elisp-complete--add-enclosing-to-recent (&rest ignored)
  "Add the symbols in the enclosing top-level form to `elisp-complete--recent-syms'"
  (elisp-complete--add-to-recent (elisp-complete--read-top-level)))

(defun elisp-complete--add-preceding-to-recent (&rest ignored)
  "Add the symbols in the preceding form to `elisp-complete--recent-syms'"
  (elisp-complete--add-to-recent (elisp-complete--read-top-level)))

(advice-add #'edebug-eval-defun :after #'elisp-complete--add-enclosing-to-recent)

(advice-add #'eval-last-sexp :after #'elisp-complete--add-preceding-to-recent)

(defun elisp-complete--locals-at-point ()
  (catch 'done
    ;; Macro expand the source around point and see what bindings are
    ;; present.
    (-let* (((form-start form-end) (elisp-def--enclosing-form
                                    (elisp-def--syntax-depth)))
            (placeholder (elisp-def--fresh-placeholder))
            (src (elisp-def--source-with-placeholder form-start form-end placeholder))
            (form (condition-case nil
                      (read src)
                    (end-of-file nil)))
            (expanded-form (macroexpand-all form)))
      ;; TODO: this includes generated symbols produced by dash macro expansion.
      (elisp-def--bound-syms expanded-form placeholder))))

(defun elisp-complete--annotate (str desc)
  "Mutate STR to apply DESC as a text property.
Returns STR."
  (put-text-property 0 1 'description desc str)
  str)

(defun elisp-complete--callables (prefix)
  "Return a list of symbol names of callables starting with PREFIX.

Each symbol is a function, macro or special form. Symbols that
have been recently used are ordered first."
  (let* (
         ;; Recently seen symbols that are functions.
         (used-fn-syms (--filter
                        (and (fboundp it)
                             (s-prefix-p prefix (symbol-name it)))
                        elisp-complete--recent-syms))
         ;; Offer other functions that start with this prefix, but
         ;; only after recently used symbols.
         (all-fn-syms (elisp-complete--all-callable-syms prefix))
         (unused-fn-syms
          (--filter (not (memq it used-fn-syms)) all-fn-syms)))
    ;; The used fns are already ordered by most recently used first,
    ;; but we want to sort the remainder alphabetically.
    (setq unused-fn-syms (sort unused-fn-syms #'string<))

    (--map                              ;
     (elisp-complete--annotate
      (copy-sequence (symbol-name it))
      (cond
       ((functionp it) 'defun)
       ((macrop it) 'defmacro)
       (t 'special-form)))
     (append used-fn-syms unused-fn-syms))))

(defun elisp-complete--vars (prefix)
  (let* ((fn-syms (-filter #'boundp elisp-complete--recent-syms))
         (sym-names (-map #'symbol-name fn-syms))
         ;; TODO: this is just showing all matching, recent vars,
         ;; regardless of whether they're appropriate.
         (matching-names
          (--filter (s-starts-with-p prefix it) sym-names))
         (local-names
          (-map #'symbol-name (elisp-complete--locals-at-point)))
         (matching-locals
          (--filter (s-starts-with-p prefix it) local-names)))
    (append
     (--map (elisp-complete--annotate it 'let) matching-locals)
     matching-names)))

(defun elisp-complete--candidates (prefix)
  ;; TODO: this isn't a great fit, because
  ;; `elisp-def--namespace-at-point' distinguishes bound vars from
  ;; global vars. We don't care here and it's needless work.
  (let ((namespace (elisp-def--namespace-at-point)))
    (cond
     ((eq namespace 'function)
      (elisp-complete--callables prefix))
     ((memq namespace '(variable bound))
      (elisp-complete--vars prefix))
     (t
      (append
       ;; Put vars first, as that ensures we have locals and params
       ;; earlier.
       (elisp-complete--vars prefix)
       (elisp-complete--callables prefix))))))

(defun elisp-complete--format-annotation (candidate)
  (-when-let (description (get-text-property 0 'description candidate))
    (format " %s" description)))

(defun elisp-complete--docstring (sym)
  (ignore-errors
    ;; TODO: split first line/sentence of docstring.

    ;; TODO: ignore-errors is crude. I think we just need to check
    ;; it's bound.
    (documentation sym)))

(defun elisp-complete (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'elisp-complete))
    (`prefix (company-grab-symbol))
    (`candidates (elisp-complete--candidates arg))
    (`meta (elisp-complete--docstring (intern arg)))
    (`annotation (elisp-complete--format-annotation arg))))

(provide 'elisp-complete)
;;; elisp-complete.el ends here
