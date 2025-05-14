;;; package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install use-package))



;;; redirect tempfile and customization spam
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq disabled-command-function nil
      custom-file (concat user-emacs-directory "custom.el"))
(message custom-file)
(load custom-file 'noerror)



;;; editor and major mode preferences
(setq-default tab-width 4
              indent-tabs-mode nil)
(setq-default c-default-style
              '((c-mode . "k&r")
                (c++-mode . "k&r")
                (csharp-mode . "bsd")
                (other . "java")))



;;; keyboard bindings
(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))



;;; appearance
(setq column-number-mode t)
(use-package nordic-night-theme
  :ensure t
  :config
  (load-theme 'nordic-night t))
