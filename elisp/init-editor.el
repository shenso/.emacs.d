;;;; general emacs/editor configurations
(require 'org)

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

;; obey column 80 limit
(setq-default fill-column 80)

;; alarm bell
(setq visible-bell 1)

;;; editor packages & additions
(use-package general)
;; the mark of the beast...
(use-package evil
  :after (general)
  :config
  (org-babel-load-file "~/.emacs.d/elisp/config/evil.org")
  (evil-mode 1))

(load "editor/backend")
(load "editor/integrations")
(load "editor/navigation")

;; scripts for language major modes
(load "lang/setup-cc-mode")
(load "lang/setup-csharp-mode")
(load "lang/setup-lua-mode")
