;;; package --- Summary

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
  :config
  (general-evil-setup))

(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq-default evil-symbol-word-search t)

  (add-hook 'org-mode-hook (lambda () (setq-local evil-auto-indent nil)))

  (general-nmap "-" #'dired-jump)

  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "e" #'find-file)

  (general-nmap
    :keymaps 'dired-mode-map
    "-" #'dired-up-directory)

  (general-nmap
    :prefix ","
    "d" #'delete-trailing-whitespace)

  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(evil-define-operator chp-evil-eval (beg end type)
  :move-point nil
  (eval-region beg end t))

(use-package elisp-mode
  :init
  (general-mmap
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    ", e" #'chp-evil-eval))

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
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "g" #'magit)
  (evil-ex-define-cmd "Gblame" #'magit-blame-addition))

(use-package evil-magit
  :ensure t)

(use-package paredit
  :ensure t
  :init
  (let ((modes '(emacs-lisp-mode-hook
                 clojure-mode-hook
                 cider-repl-mode-hook
                 fennel-mode-hook
                 inferior-lisp-mode-hook)))
    (dolist (hook modes)
      (add-hook hook #'paredit-mode))))

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
  (add-to-list 'evil-change-commands #'evil-cp-change))

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
  :init (global-flycheck-mode 1))

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

(use-package lua-mode
  :ensure t)

(use-package fennel-mode
  :ensure t
  :init
  (general-nmap
    :keymaps 'fennel-mode-map
    "g z" #'fennel-repl
    "g d" #'fennel-find-definition))

(provide 'init)
;;; init.el ends here
