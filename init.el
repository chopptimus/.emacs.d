;; General
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

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
  (evil-mode 1))

(use-package helm
   :ensure t
   :pin melpa-stable
   :init
   (progn
     (require 'helm-config)
     (helm-mode))
   :bind
   ("M-x" . helm-M-x)
   ("C-h a" . helm-apropos))

(use-package projectile
  :ensure t)

(use-package cider
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

;; Hooks
(defun add-hook-m (hs fn) (mapc (lambda (h) (add-hook h fn)) hs))

(defun my-prog-mode ()
  (linum-mode)
  (lambda () (modify-syntax-entry ?_ "w")))

(defun my-lisp-mode ()
  (modify-syntax-entry ?- "w")
  (rainbow-delimiters-mode))

(add-hook-m '(clojure-mode-hook emacs-lisp-mode-hook) 'my-lisp-mode)
(add-hook 'prog-mode-hook 'my-prog-mode)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
