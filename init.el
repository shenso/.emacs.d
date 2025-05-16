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
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (global-set-key (kbd "C-c [") (lambda () (interactive) (turn-on-evil-mode) (evil-normal-state)))
  (global-set-key (kbd "C-c ]") 'turn-off-evil-mode))

(use-package evil-collection
  :ensure t
  :after (evil magit)
  :config
  (let ((target-collections '(dired)))
    (when (memq 'magit package-activated-list)
      (push 'magit  target-collections))
    (evil-collection-init target-collections)))

;; resize keybinds
(global-set-key (kbd "C-c j") 'shrink-window)
(global-set-key (kbd "C-c k") 'enlarge-window)
(global-set-key (kbd "C-c h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c l") 'enlarge-window-horizontally)



;;; integrations
(use-package vterm
  :ensure t
  :after evil
  :hook (vterm-mode . turn-off-evil-mode)
  :config
  (defun handle-vterm-switch (&optional dummy)
    (when (equal major-mode 'vterm-mode)
      (call-interactively 'turn-off-evil-mode)))
  (when (memq 'evil package-activated-list)
    (add-hook 'window-selection-Change-Functions 'handle-vterm-switch)))

(use-package magit
  :ensure t)



;;; project management
(use-package projectile
  :ensure t
  :init
  (cl-case system-type
    ((gnu/linux quote)
     (setq projectile-key (kbd "C-c p")
           projectile-project-search-path '("~/projects")))
    ((darwin quote)
     (setq projectile-key (kbd "s-p")
           projectile-project-search-path '("~/Projects"))))
  :config
  (projectile-global-mode)
  (projectile-discover-projects-in-search-path)
  (define-key projectile-mode-map projectile-key 'projectile-command-map))



;;; major modes
(use-package yaml-mode
  :ensure t)
(use-package csv-mode
  :ensure t
  :hook ((csv-mode tsv-mode) . csv-align-mode))
(use-package typescript-mode
  :if (equal system-type 'darwin)
  :ensure t)



;;; appearance
(tool-bar-mode -1)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package nordic-night-theme
  :ensure t
  :config
  (load-theme 'nordic-night t))
