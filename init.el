(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

(setq-default indent-tabs-mode nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode 1)
(column-number-mode)

(setq ido-everywhere t)
(ido-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
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
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package evil-cleverparens
  :ensure t
  :init
  (add-hook 'paredit-mode-hook #'evil-cleverparens-mode))

(use-package cider
  :ensure t)
