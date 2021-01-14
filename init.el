;;; init.el --- Strong message here  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs configuration

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(savehist-mode)
(setq column-number-indicator-zero-based nil)
(column-number-mode)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(save-place-mode)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package paren
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode))

(use-package general
  :ensure t
  :demand t)

(general-evil-setup)

(general-def 'normal "C-;" #'eval-expression)

(general-def "M-u" #'universal-argument)

(general-nmap "-" #'dired-jump)

(general-nmap
  :keymaps 'dired-mode-map
  "-" #'dired-up-directory)

;; Prefer this to counsel-find-file
(ido-mode 'files)
(general-nmap :keymaps 'override "SPC e" #'ido-find-file)

(general-nmap
  :prefix "SPC n"
  "w" #'widen
  "d" #'narrow-to-defun
  "x" #'sp-narrow-to-sexp)

(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil ; evil-collection takes care of this
        evil-want-C-u-scroll t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  (setq-default evil-symbol-word-search t)

  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode))

(use-package org
  :init
  (general-nmap
    :keymaps 'org-mode-map
    :prefix "SPC n"
    "s" #'org-narrow-to-subtree
    "b" #'org-narrow-to-block
    "e" #'org-narrow-to-element)
  (general-nmap
    :prefix "SPC o"
    "a" #'org-agenda
    "c" #'counsel-org-capture))

(evil-define-operator chp-evil-eval (beg end type)
  :move-point nil
  (eval-region beg end t))

(use-package elisp-mode
  :init
  (general-mmap
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    ", e" #'chp-evil-eval
    "C-c C-c" #'eval-defun
    "C-c C-k" #'eval-buffer)

  (general-define-key
   :states '(normal insert)
   :keymaps 'lisp-interaction-mode-map
   "C-j" #'eval-print-last-sexp))

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo-tree-history/")))
  :config
  (global-undo-tree-mode))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package magit
  :ensure t
  :after evil
  :init
  (evil-ex-define-cmd "G" #'magit)
  (evil-ex-define-cmd "Gstatus" #'magit)
  (evil-ex-define-cmd "Gblame" #'magit-blame-addition)
  (general-nmap
    :keymaps 'magit-mode-map
    ", c c" (lambda ()
              (interactive)
              (magit-run-git "commit" "-m" "Checkpoint"))))

(use-package paredit
  :ensure t
  :init
  (general-add-hook '(emacs-lisp-mode-hook
                      clojure-mode-hook
                      cider-repl-mode-hook
                      fennel-mode-hook
                      inferior-lisp-mode-hook
                      eval-expression-minibuffer-setup-hook)
                    #'paredit-mode)
  (general-nmap
    :keymaps 'paredit-mode-map
    :prefix ","
    "\S-o" #'paredit-raise-sexp
    "@" #'paredit-splice-sexp))

(use-package evil-cleverparens
  :ensure t
  :diminish
  :init
  (add-hook 'paredit-mode-hook #'evil-cleverparens-mode)
  (general-nmap
    :keymaps 'evil-cleverparens-mode-map
    :prefix ","
    "w r" #'evil-cp-wrap-next-round
    "w s" #'evil-cp-wrap-next-square
    "w c" #'evil-cp-wrap-next-curly)
  (setq evil-cleverparens-use-regular-insert t)
  :config
  (require 'evil-cleverparens-fixes)
  (add-to-list 'evil-change-commands #'evil-cp-change)
  (general-define-key
   :states '(normal visual operator)
   :keymaps 'evil-cleverparens-mode-map
   "{" nil
   "}" nil
   "[" nil
   "]" nil
   "[ s" #'evil-cp-previous-opening
   "] s" #'evil-cp-next-closing
   "[ d" #'evil-cp-next-opening
   "] d" #'evil-cp-previous-closing))

(evil-define-operator chp-evil-cider-eval (beg end type)
  :move-point nil
  (cider-interactive-eval nil
                          nil
                          (list beg end)
                          (cider--nrepl-pr-request-map)))

(use-package clojure-mode
  :ensure t
  :init
  (general-nmap
    :keymaps 'clojure-mode-map
    :prefix ", j"
    "j" #'cider-jack-in
    "c" #'cider-jack-in-cljs
    "b" #'cider-jack-in-clj&cljs)
  (general-nmap
    :keymaps 'clojure-mode-map
    :prefix ", c"
    "j" #'cider-connect
    "s" #'cider-connect-cljs
    "b" #'cider-connect-clj&cljs))

(use-package cider
  :ensure t
  :init
  (general-mmap :keymaps 'cider-mode-map ", e" #'chp-evil-cider-eval)
  (general-nmap
    :keymaps 'cider-mode-map
    :prefix ", c"
    "i" #'cider-inspect-last-result)
  (general-nmap
    :keymaps 'cider-repl-mode-map
    :prefix ", c r"
    "c" #'cider-repl-clear-buffer)
  (general-nmap
    :keymaps 'cider-repl-mode-map
    "g o" #'cider-repl-switch-to-other)
  (add-hook 'cider-mode-hook #'eldoc-mode) ; Shouldn't be necessary, but it is.
  :config
  (require 'chp-cider-utils))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode))

(use-package counsel
  :ensure t
  :diminish
  :config
  (counsel-mode))

(use-package projectile
  :ensure t
  :diminish
  :init
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "f" #'projectile-find-file))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package evil-org
  :ensure t
  :diminish
  :init
  (add-hook 'org-mode-hook #'evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (general-nmap
    :keymaps 'flycheck-mode-map
    "] q" #'flycheck-next-error
    "[ q" #'flycheck-previous-error)
  :config
  (defun flycheck-mode-line-status-text (&optional status)
    "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or nil.

This version doesn't show a mode-line entry when there are no
checkers"
    (let ((s (or status flycheck-last-status-change)))
      (if (eq s `no-checker)
          ""
          (let ((text (pcase s
                        ('not-checked "")
                        ;; (`no-checker "-")
                        ('running "*")
                        ('errored "!")
                        ('finished
                         (let-alist (flycheck-count-errors
                                     flycheck-current-errors)
                           (if (or .error .warning)
                               (format ":%s|%s" (or .error 0) (or .warning 0))
                             "")))
                        ('interrupted ".")
                        ('suspicious "?"))))
            (concat " " flycheck-mode-line-prefix text))))))

(use-package flycheck-clj-kondo
  :ensure t
  :config (require 'flycheck-clj-kondo))

;; On macOS Emacs can't find many user installed prorams because GUI
;; apps are launched with a minimal set of environment variables.
(use-package exec-path-from-shell
  :if (eq window-system 'ns)
  :config
  (exec-path-from-shell-initialize))

(use-package rust-mode
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :init
  (with-eval-after-load 'rust-mode
   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init)
;;; init.el ends here
