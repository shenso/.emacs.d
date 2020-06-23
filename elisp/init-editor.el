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

(use-package general)
;; the mark of the beast...
(use-package evil
  :after general
  :config
  (load "config/evil")
  (evil-mode 1))

;; file tree
;; I haven't been able to resolve performance problems that neotree seems to
;; have on windows, ergo this conditional for the time being.
(use-package neotree
  :unless (string= system-type "windows-nt")
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

;; obey column 80 limit
(setq-default fill-column 80)

;; magit
(use-package magit
  :requires general
  :after (evil general)
  :config
  (when (featurep 'evil)
    (setup-magit-evil-bindings))
  :general
  (:states '(normal)
   :prefix "SPC" "gg" 'magit-status))

;; projectile
(use-package projectile
  :config
  (projectile-global-mode)
  :general
  ("p" 'projectile-command-map
   :prefix "SPC"
   :states 'normal))

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

;; company-mode (auto-complete)
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

;; flycheck (syntax checker)
(use-package flycheck)

;; language editing configurations
(load "lang/setup-cc-mode")
(load "lang/setup-csharp-mode")
(load "lang/setup-lua-mode")
