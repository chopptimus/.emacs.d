(add-to-list 'load-path "~/.emacs.d/lisp")

(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(savehist-mode 1)
(show-paren-mode 1)
(setq column-number-indicator-zero-based nil)
(column-number-mode 1)
(ido-mode 1)

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

(use-package general
  :ensure t
  :config
  (general-evil-setup))

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

  (general-nmap "-" 'dired-jump)

  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "e" 'find-file)

  (general-nmap
    :keymaps 'dired-mode-map
    "-" 'dired-up-directory)

  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package magit
  :ensure t
  :init
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "g" 'magit))

(use-package evil-magit
  :ensure t)

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode))

(use-package evil-cleverparens
  :ensure t
  :init
  (add-hook 'paredit-mode-hook #'evil-cleverparens-mode)
  :config
  (require 'evil-cp-cw)
  (add-to-list 'evil-change-commands #'evil-cp-change))

(use-package cider
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package projectile
  :ensure t
  :init
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "f" 'projectile-find-file))

(use-package evil-org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'evil-org-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode 1))

(use-package flycheck-clj-kondo
  :ensure t
  :config (require 'flycheck-clj-kondo))
