;;; chp-cider-utils.el --- Cider utils  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Utility functions for cider.

;;; Code:
(require 'cider-client)

(defun chp-cider-def-args ()
  "Insert a form defining all the args for debugging.
Doesn't work for multiple arity functions."
  (interactive)
  (save-excursion
    (forward-char)
    (beginning-of-defun)
    (forward-char)
    (forward-sexp 2)
    ; Heuristic for skipping over return type hints
    (when (< 64 (string-to-char (substring (current-word) 0 1)) 91)
      (forward-sexp))
    (let* ((args (nrepl-dict-get
                  (cider-var-info (current-word))
                  "arglists-str"))
           (syms (seq-remove
                  (lambda (s)
                    (member (string-to-char (substring s 0 1)) '(?: ?^)))
                  (split-string
                   (apply #'string
                          (seq-remove
                           (lambda (c) (memq c '(?\[ ?\] ?\{ ?\})))
                           args))
                   " " t))))
      (while (not (= (preceding-char) 93))
        (forward-sexp))
      (dolist (sym syms)
        (newline)
        (indent-for-tab-command)
        (insert "(def " sym " " sym ")"))
      (indent-for-tab-command))))

(provide 'chp-cider-utils)
;;; chp-cider-utils.el ends here
