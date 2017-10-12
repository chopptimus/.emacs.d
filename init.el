(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-wants-C-u-scroll t)
  (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "C-c") (kbd "<escape>")))

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme 'atom-one-dark t))

(global-linum-mode)
(setq inhibit-startup-message t)
(set-default-font "Source Code Pro-12")
