

(ert-deftest elisp-complete--matches ()
  ;; Prefix
  (should
   (elisp-complete--matches "foo" 'foobar))
  ;; Prefix with dashes.
  (should
   (elisp-complete--matches "--map-i" '--map-indexed))
  ;; Not matching.
  (should
   (not (elisp-complete--matches "baz" 'foobar)))
  ;; Lisp abbreviations.
  (should
   (elisp-complete--matches "w-t-b" 'with-temp-buffer)))
