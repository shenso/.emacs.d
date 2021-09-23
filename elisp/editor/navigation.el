;; file tree
(use-package treemacs
  :ensure t
  :config
  (use-package treemacs-evil
    :ensure t
    :after evil)
  (use-package treemacs-projectile
    :ensure t
    :after projectile)
  (use-package treemacs-magit
    :ensure t
    :after magit)
  :config
  :general
  ("<f8>" 'treemacs
   :keymaps 'global-map)
  (:prefix "c"
   :state 'treemacs
   :keymaps 'treemacs-mode-map
   "p a" 'treemacs-add-project-to-workspace
   "p d" 'treemacs-remove-project-from-workspace
   "p r" 'treemacs-rename-project
   "p c c" 'treemacs-collapse-project
   "p c o" 'treemacs-collapse-all-projects))

;; projectile
(use-package projectile
  :ensure t
  ;; :after neotree			
  :init
  (setq projectile-switch-project-action
	(lambda ()
	  (cd (projectile-project-root))
	  ;; (neotree-projectile-action)
	  ))
  (load "config/projectile")
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
