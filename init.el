(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)

(setq-default indent-tabs-mode nil)

(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(ido-mode 1)
(ido-everywhere 1)

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
  (setq evil-want-C-u-scroll t
        evil-split-window-below t
        evil-vsplit-window-right t)
  (setq-default evil-symbol-word-search t)

  (general-nmap "-" 'dired-jump)

  (general-nmap
    :prefix "SPC"
    "e" 'find-file)

  (general-nmap
    :keymaps 'dired-mode-map
    "-" 'dired-up-directory)

  (general-mmap
    :keymaps 'Info-mode-map
    "n" 'evil-search-next
    "C-n" 'Info-next
    "C-p" 'Info-prev)

  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package magit
  :ensure t
  :init
  (general-nmap
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
  (add-hook 'paredit-mode-hook #'evil-cleverparens-mode))

(defun cider-evil-eval-last-sexp ()
  (interactive)
  (forward-char)
  (cider-eval-last-sexp)
  (backward-char))

(defun evil-cider-doc ()
  (setq-local evil-lookup-func 'cider-doc))

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'evil-cider-doc)
  (add-hook 'cider-repl-mode-hook 'evil-cider-doc)

  (general-nmap
    :keymaps 'cider-mode-map
    "C-x C-e" 'cider-evil-eval-last-sexp
    "C-c C-e" 'cider-evil-eval-last-sexp)

  (general-nmap
    :keymaps '(cider-mode-map cider-repl-mode-map)
    :prefix "SPC"
    "d" 'cider-find-var))

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
