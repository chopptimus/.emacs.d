;; General
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq-default indent-tabs-mode nil)

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
  (progn
    (setq evil-want-C-u-scroll t)
    (evil-mode 1))
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-c C-x") 'evil-delete-buffer)
    (define-key evil-insert-state-map (kbd "C-c") 'evil-force-normal-state)
    (define-key evil-visual-state-map (kbd "C-c") 'evil-force-normal-state)))

(use-package helm
   :ensure t
   :diminish helm-mode
   :pin melpa-stable
   :init
   (progn
     (require 'helm-config)
     (setq helm-ff-skip-boring-files t)
     (helm-mode))
   :bind
   ("C-c h" . helm-mini)
   ("M-x" . helm-M-x)
   ("C-h a" . helm-apropos))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :bind
  ("C-c p p" . helm-projectile-switch-project))

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

(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
