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

;; TODO: Push recently accepted completions to the head of this list too.
(defvar elisp-complete--recent-syms nil)
(defvar elisp-complete--history-size 1000)

(defun elisp-complete--all-callable-syms (prefix)
  "Return a list of all bound syms that start with PREFIX."
  (let (syms)
    (mapatoms
     (lambda (sym)
       (when (and
              (fboundp sym)
              (elisp-complete--matches prefix sym))
         (push sym syms))))
    syms))

(defun elisp-complete--matches (prefix sym)
  "Does SYM match PREFIX?

SYM may be a symbol or a string. PREFIX may optionally be
abbreviated, for example w-t-b for with-temp-buffer."
  (when (symbolp sym)
    (setq sym (symbol-name sym)))
  (let ((sym-parts (s-split (rx "-") sym))
        (prefix-parts (s-split (rx "-") prefix)))
    (and
     (<= (length prefix-parts) (length sym-parts))
     (-all-p
      (-lambda ((sym-part . prefix-part))
        (s-starts-with-p prefix-part sym-part))
      (-zip-pair sym-parts prefix-parts)))))

(elisp-complete--matches "w-t-c" 'with-temp-buffer)

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

;; TODO: actually get preceding form.
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
  "Return a copy of STR with DESC as a text property."
  (setq str (copy-sequence str))
  (put-text-property 0 1 'description desc str)
  str)

(cl-defun elisp-complete-cands (prefix &key (funcs t) (macros t) (vars t))
  (let* (
         ;; Symbols that we can see are lexically bound.
         (local-syms (and vars (elisp-complete--locals-at-point)))
         (local-syms
          (--filter (elisp-complete--matches prefix it) local-syms))
         ;; Recently used, globally bound symbols.
         (recent-syms
          (--filter
           (or
            (and funcs (functionp it))
            (and macros (macrop it))
            (and vars (boundp it)))
           elisp-complete--recent-syms))
         (recent-syms
          (--filter (elisp-complete--matches prefix it) recent-syms))

         bound-syms unused-syms global-syms)
    ;; Find all bound symbols in the namespaces we're looking at.
    (mapatoms
     (lambda (sym)
       (when
           (or
            (and funcs (functionp sym))
            (and macros (macrop sym))
            (and vars (boundp sym)))
         (push sym bound-syms))))
    ;; Filter bound symbols to unused symbols matching the current prefix.
    (setq bound-syms
          (--filter (elisp-complete--matches prefix it) bound-syms))
    (setq unused-syms
          (--filter (not (memq it recent-syms)) bound-syms))
    ;; Sort unused symbols alphabetically.
    (setq unused-syms (sort unused-syms #'string<))
    ;; Offer recently used global symbols before other global symbols.
    (setq global-syms (append recent-syms unused-syms))

    (append
     (--map
      ;; TODO: distinguish local variables from parameters.
      (elisp-complete--annotate (symbol-name it) 'local)
      local-syms)
     (--map
      (elisp-complete--annotate
       (symbol-name it)
       (cond
        ;; If it's both a variable and a function, and we're looking
        ;; for both, show this in the annotation.
        ((and (fboundp it) (boundp it) funcs vars)
         'defvar/defun)
        ;; Otherwise, annotate with the first namespace found.
        ((and (macrop it) macros)
         'defmacro)
        ;; TODO: it would be nice to distinguish defsubst.
        ((and (fboundp it) funcs)
         'defun)
        ((and (boundp it) vars)
         'defvar)))
      global-syms))))

(defun elisp-complete--candidates (prefix)
  ;; TODO: this isn't a great fit, because
  ;; `elisp-def--namespace-at-point' distinguishes bound vars from
  ;; global vars. We don't care here and it's needless work.
  (let ((namespace (elisp-def--namespace-at-point)))
    (cond
     ((eq namespace 'function)
      (elisp-complete-cands prefix :vars nil))
     ((memq namespace '(variable bound))
      (elisp-complete-cands prefix :funcs nil :macros nil))
     (t
      (elisp-complete-cands prefix)))))

(defun elisp-complete--format-annotation (candidate)
  (-when-let (description (get-text-property 0 'description candidate))
    (format " %s" description)))

;; TODO: write tests for this, particularly handling
;; syntax-ppss-depth.
(defun elisp-complete--docstring (sym)
  (when (or (boundp sym) (fboundp sym))
    (-when-let (docstring (documentation sym))
      (concat
       ;; Just show the first sentence.
       (car (s-split-up-to (rx ".") docstring 1))
       "."))))

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
