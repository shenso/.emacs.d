;; general emacs/editor configurations

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
(prefer-coding-system 'utf-8)

;; file tree
(use-package neotree
  :config
  (setq neo-theme 'arrow)
  (global-set-key [f8] 'neotree-toggle))

;; obey column 80 limit
(setq-default fill-column 80)

;; magit
(use-package magit)

;; sudo-edit
(use-package sudo-edit)

;; alarm bell
(setq visible-bell 1)

;; language editing configurations
(load "lang/setup-cc-mode")
(load "lang/setup-csharp-mode")
(load "lang/setup-lua-mode")
