;;;; general emacs/editor configurations

;; move backup files to ~/.emacs.d/.backups
(setq backup-directory-alist `(("." . "~/.emacs.d/.backups"))
      backup-by-copying t
      ;; handle old backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; encoding
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; file tree
;; I haven't been able to resolve performance problems that neotree seems to
;; have on windows, ergo this conditional for the time being.
(unless (string= system-type "windows-nt")
  (use-package neotree
    :requires all-the-icons
    :bind ("<f8>" . 'neotree-toggle)
    :config
    (setq neo-theme 'arrow)))

;; obey column 80 limit
(setq-default fill-column 80)

;; magit
(use-package magit
  :bind ("C-x g" . 'magit-status))

;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode))

;; dashboard
(use-package dashboard
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

;; sudo-edit
(use-package sudo-edit)

;; alarm bell
(setq visible-bell 1)

;; language editing configurations
(load "lang/setup-cc-mode")
(load "lang/setup-csharp-mode")
(load "lang/setup-lua-mode")
