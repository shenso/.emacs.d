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

(use-package undo-tree ;; this is stupid
  :ensure t
  :config
  (global-undo-tree-mode))
(use-package evil
  :ensure t
  :after (general undo-tree)
  :init
  (load "config/evil")
  (setq evil-undo-system 'undo-tree)
  (setup-org-evil-bindings)
  :config
  (evil-mode t)
  :general
  ("?"   'describe-mode
   "TAB" 'indent-for-tab-command
   "u"   'undo-tree-undo
   "C-r" 'undo-tree-redo
   :states '(normal visual)))

(load "editor/integrations")
(load "editor/navigation")
(load "editor/langs")
(load "editor/autocomplete")
(load "editor/linter")
