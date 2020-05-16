(require 'evil)

;; https://github.com/luxbock/evil-cleverparens/issues/59
(defun evil-cp--forward-X-begin (thing count)
  "TODO: see `forward-evil-cp-word' which is currently not
working. Could be used to implement a future
`evil-cp-forward-word-begin' the same way that
`evil-cp-forward-symbol-begin' is defined."
  (let ((orig (point)))
    (evil-signal-at-bob-or-eob count)
    (cond ((not (evil-operator-state-p))
           (evil-forward-beginning thing count))

          ((and evil-want-change-word-to-end
                ;; This is the changed line.
                (memq evil-this-operator evil-change-commands)
                (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
           (forward-thing thing count))

          (t (prog1 (evil-forward-beginning thing count)
               (when (and (> (line-beginning-position) orig)
                          (looking-back "^[[:space:]]*" (line-beginning-position)))
                 (evil-move-end-of-line 0)
                 (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                             (not (<= (line-beginning-position) orig)))
                   (evil-move-end-of-line 0))
                 (when (bolp) (forward-char))))))))

(provide 'evil-cp-cw)
