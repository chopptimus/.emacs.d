(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode 1))

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme 'atom-one-dark t))

;; Ease some of the gui pain
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(set-default-font "Source Code Pro-15")

;; General
(global-linum-mode)
