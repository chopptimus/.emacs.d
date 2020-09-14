;;; chp-cider-utils.el --- Cider utils  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Utility functions for cider.

;;; Code:
(require 'parseedn)

(defun chp-cider-def-args ()
  "Insert a form defining all the args for debugging.
Doesn't work for multiple arity functions."
  (interactive)
  (save-excursion
    (let ((form (save-mark-and-excursion
                  (mark-defun)
                  (parseedn-read-str (buffer-substring-no-properties
                                      (region-beginning)
                                      (region-end))))))
      (beginning-of-defun)
      (forward-char)
      (while (not (vectorp (car form)))
        (setq form (cdr form))
        (forward-sexp))
      (forward-sexp)
      (seq-doseq (arg (car form))
        (newline)
        (indent-for-tab-command)
        (let ((s (symbol-name arg)))
          (insert "(def " s " " s ")")))
      (indent-for-tab-command))))

(provide 'chp-cider-utils)
;;; chp-cider-utils.el ends here
