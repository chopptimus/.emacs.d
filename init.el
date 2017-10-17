;; General
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

(setq-default indent-tabs-mode nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; ui
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (progn
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-c C-x") 'evil-delete-buffer)
    (define-key evil-insert-state-map (kbd "C-c") (kbd "<escape>"))
    (define-key evil-visual-state-map (kbd "C-c") (kbd "<escape>"))))

;; sensible undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package cider
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme 'atom-one-dark t))

;; Hooks
(defun add-hook-m (hs fn) (mapc (lambda (h) (add-hook h fn)) hs))

(defun my-prog-mode ()
  (linum-mode)
  (modify-syntax-entry ?_ "w"))

(defun my-lisp-mode ()
  (modify-syntax-entry ?- "w")
  (rainbow-delimiters-mode))

(add-hook-m '(clojure-mode-hook emacs-lisp-mode-hook) 'my-lisp-mode)
(add-hook 'prog-mode-hook 'my-prog-mode)
