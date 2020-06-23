;; file tree
;; I haven't been able to resolve performance problems that neotree seems to
;; have on windows, ergo this conditional for the time being.
(use-package neotree
  :unless (string= system-type "windows-nt")
  :ensure t
  :requires (all-the-icons general)
  :after (evil general)
  :config
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil)
  (setq neo-hidden-regexp-list '("\\.meta$"))
  (when (featurep 'evil)
    (setup-neotree-evil-bindings))
  :general
  ("<f8>" 'neotree-toggle))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  :general
  ("p" 'projectile-command-map
   :prefix "SPC"
   :states 'normal))

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title (concat "emacs@" (system-name))
	dashboard-items '((bookmarks . 5)
			  (projects . 5)
			  (recents . 5)
			  (agenda . 10))
	dashboard-set-heading-icons t
	dashboard-set-file-icons t)
  ;; no image for windows for now
  (unless (string= system-type "windows-nt")
    (setq dashboard-startup-banner "~/images/anime/lain/lain5.png"))
  (dashboard-setup-startup-hook))
