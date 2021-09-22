;;;; general emacs/editor configurations
(require 'org)
(require 'packaging)

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
(use-package general
  :ensure t)
;; the mark of the beast...
(use-package evil
  :ensure t
  :after general
  :init
  (load "config/evil")
  (setup-org-evil-bindings)
  :config
  (evil-mode t)
  :general
  ("?"   'describe-mode
   "TAB" 'indent-for-tab-command
   :states '(normal visual)))

(load "editor/backend")
(load "editor/integrations")
(load "editor/navigation")

(add-to-list 'load-path
	     (concat scripts-directory "lang/"))

;; scripts for language major modes
(load "setup-cc-mode")
(load "setup-csharp-mode")
(load "setup-lua-mode")
(load "setup-php-mode")
(load "setup-python-mode")
(load "setup-typescript-mode")
(load "setup-web-mode")
(load "setup-misc-modes")
