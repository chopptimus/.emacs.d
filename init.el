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
(savehist-mode 1)
(setq column-number-indicator-zero-based nil)
(column-number-mode 1)
(ido-mode 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(global-eldoc-mode)

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
  (show-paren-mode 1))

(use-package general
  :ensure t
  :demand t)

(general-evil-setup)

(general-def 'normal "C-;" #'eval-expression)

(general-def "M-u" #'universal-argument)

(general-nmap
  :prefix "SPC s"
  "s" #'shell-command
  "a" #'async-shell-command
  "p" (lambda ()
        (interactive)
        (shell-command (car shell-command-history)))
  "o" (lambda ()
        (interactive)
        (async-shell-command (car shell-command-history))))

(general-nmap "-" #'dired-jump)

(general-nmap
  :keymaps 'dired-mode-map
  "-" #'dired-up-directory)

(general-nmap :keymaps 'override "SPC e" #'find-file)

(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-respect-visual-line-mode t)
  (setq-default evil-symbol-word-search t)

  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package org
  :init
  (add-hook 'org-mode-hook (lambda () (setq-local evil-auto-indent nil))))

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
  :diminish)

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
  (evil-ex-define-cmd "Gblame" #'magit-blame-addition))

(use-package evil-magit
  :ensure t)

(use-package paredit
  :ensure t
  :init
  (general-add-hook '(emacs-lisp-mode-hook
                      clojure-mode-hook
                      cider-repl-mode-hook
                      fennel-mode-hook
                      inferior-lisp-mode-hook
                      eval-expression-minibuffer-setup-hook)
                    #'paredit-mode))

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

(use-package cider
  :ensure t
  :init
  (general-mmap
    :keymaps 'cider-mode-map
    ", e" #'chp-evil-cider-eval))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))

(use-package projectile
  :ensure t
  :init
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "f" #'projectile-find-file))

(use-package evil-org
  :ensure t
  :diminish
  :init
  (add-hook 'org-mode-hook #'evil-org-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1)
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

(use-package cc-mode
  :init
  (general-nmap
    :keymaps 'c-mode-map
    :prefix ","
    "r" #'compile
    "m" #'recompile))

(use-package adaptive-wrap
  :ensure t)

(use-package adoc-mode
  :ensure t)

(use-package visual-fill-column
  :ensure t
  :init
  (let ((mode-hooks (list #'visual-line-mode
                          #'visual-fill-column-mode
                          #'adaptive-wrap-prefix-mode)))
    (general-add-hook 'text-mode-hook mode-hooks)

    (defun toggle-visual-line-modes ()
      (interactive)
      (let ((x (if (symbol-value (car mode-hooks)) -1 1)))
        (dolist (f mode-hooks)
          (funcall f x))))

    (general-nmap
      :keymaps '(text-mode-map adoc-mode-map)
      :prefix ","
      "v t" #'toggle-visual-line-modes)))

(provide 'init)
;;; init.el ends here
