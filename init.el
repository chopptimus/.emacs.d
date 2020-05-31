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
  (setq evil-want-C-u-scroll t
        evil-split-window-below t
        evil-vsplit-window-right t)
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

  (general-mmap
    :keymaps 'Info-mode-map
    "n" 'evil-search-next
    "C-n" 'Info-next
    "C-p" 'Info-prev)

  (general-mmap
    :keymaps 'grep-mode-map
    "n" 'evil-search-next
    "C-n" 'next-error-no-select
    "C-p" 'previous-error-no-select)

  (general-mmap
    :keymaps 'xref--xref-buffer-mode-map
    "," 'xref-prev-line)

  (general-mmap
    :keymaps 'help-mode-map
    "TAB" 'forward-button)

  :config
  (evil-set-initial-state 'xref--xref-buffer-mode 'motion)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode 1))

(use-package magit
  :ensure t
  :init
  (general-nmap
    :keymaps 'override
    :prefix "SPC"
    "g" 'magit))

(use-package evil-magit
  :ensure t
  :init
  (general-nmap
    :keymaps 'magit-mode-map
    "C-j" nil
    "C-k" nil
    "C-n" 'magit-section-forward
    "C-p" 'magit-section-backward))

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

(defun cider-evil-eval-last-sexp ()
  (interactive)
  (forward-char)
  (cider-eval-last-sexp)
  (backward-char))

(defun evil-cider-doc ()
  (setq-local evil-lookup-func 'cider-doc))

(use-package cider
  :ensure t
  :after evil
  :init
  (add-hook 'cider-mode-hook 'evil-cider-doc)
  (add-hook 'cider-repl-mode-hook 'evil-cider-doc)

  (general-nmap
    :keymaps '(cider-mode-map cider-repl-mode-map)
    :prefix "SPC"
    "d" 'cider-find-var)

  (general-define-key
   :keymaps 'cider-mode-map
   "C-x C-e" 'cider-evil-eval-last-sexp
   "C-c C-e" 'cider-evil-eval-last-sexp)

  :config
  (evil-set-initial-state 'cider-stacktrace-mode 'motion))

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
