;; file tree
(use-package neotree
  :ensure t
  :after (all-the-icons evil general projectile)
  :init
  (setq neo-theme 'arrow
	neo-window-fixed-size nil
	neo-hidden-regexp-list '("\\.meta$")) 
  :config
  (when (featurep 'evil)
    (setup-neotree-evil-bindings))
  (when (featurep 'projectile)
    (setq projectile-switch-project-action 'neotree-projectile-action))
  :general
  ("<f8>" 'neotree-toggle
   :states '(normal insert visual emacs)
   :keymap 'global-map))

;; projectile
(use-package projectile
  :ensure t
  :init (load "config/projectile")
  :config
  (projectile-global-mode)
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on))
  :general
  ("p" 'projectile-command-map
   :prefix "SPC"
   :states 'normal))

;; dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title (concat "emacs@" (system-name))
	dashboard-items '((bookmarks . 5)
			  (projects . 5)
			  (recents . 5)
			  (agenda . 10))
	dashboard-set-heading-icons t
	dashboard-set-file-icons t)
  (load "config/dashboard")
  :config
  (dashboard-setup-startup-hook))
