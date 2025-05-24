;;; package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install use-package))



;;; system lists
(setq personal-systems '("plato")
      work-systems '("smith.local"))



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
  (let ((target-collections '(dired calendar)))
    (when (memq 'magit package-activated-list)
      (push 'magit  target-collections))
    (evil-collection-init target-collections)))

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (evil-org-agenda-set-keys))

;; resize keybinds
(global-set-key (kbd "C-c j") 'shrink-window)
(global-set-key (kbd "C-c k") 'enlarge-window)
(global-set-key (kbd "C-c h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c l") 'enlarge-window-horizontally)



;;; integrations
(use-package vterm
  :ensure t
  :after evil
  :config
  (defun handle-vterm-buffer-exit ()
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (> (length (window-list)) 1)
                  (delete-window (get-buffer-window (current-buffer)))))
              nil t))

  (add-hook 'vterm-mode-hook 'handle-vterm-buffer-exit)
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-reuse-mode-window
                  display-buffer-at-bottom)
                 (window-height . 12)))

  (global-set-key (kbd "C-x vt") 'vterm)

  (when (memq 'evil package-activated-list)
    (defun handle-vterm-switch (&optional dummy)
      (when (equal major-mode 'vterm-mode)
        (call-interactively 'turn-off-evil-mode)))
    (add-hook 'vterm-mode-hook 'turn-off-evil-mode)
    (add-hook 'window-selection-change-functions 'handle-vterm-switch)))

(use-package exec-path-from-shell
  :ensure t
  :if (and (equal system-type 'darwin) (daemonp))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package magit
  :ensure t
  :after exec-path-from-shell
  :config
  ;; display git status buffer in current window
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                           (memq (with-current-buffer buffer major-mode)
                                 '(magit-process-mode
                                   magit-revision-mode
                                   magit-diff-mode
                                   magit-stash-mode
                                   magit-status-mode)))
                      nil
                    '(display-buffer-same-window))))))



;;; project management
(use-package projectile
  :ensure t
  :init
  (cl-case system-type
    ((gnu/linux quote)
     (setq projectile-key (kbd "C-c p")
           projectile-project-search-path '("~/projects")))
    ((darwin quote)
     (let ((projectile-cache-dir (expand-file-name "~/.local/cache/emacs")))
       (make-directory projectile-cache-dir :parents)
       (setq projectile-key (kbd "s-p")
             projectile-project-search-path '("~/Projects")
             projectile-cache-file (expand-file-name "projectile.cache" projectile-cache-dir)
             projectile-known-projects-file (expand-file-name "projectile-known-projects.eld" projectile-cache-dir))
      )
     ))
  :config
  (projectile-global-mode)
  (projectile-discover-projects-in-search-path)
  (define-key projectile-mode-map projectile-key 'projectile-command-map))



;;; major modes

;; prog-mode derivatives
(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode))
(use-package csv-mode
  :ensure t
  :hook ((csv-mode tsv-mode) . csv-align-mode))
(use-package typescript-mode
  :if (equal system-name "smith.local")
  :ensure t)
(use-package go-mode
  :if (equal system-name "plato")
  :ensure t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save))))

;; this sucks and has insane defaults, but it sucks less than default sql-mode
(use-package sql-indent
  :if (equal system-name "smith.local")
  :ensure t
  :config
  (defun setup-sql-indent ()
    (setq sqlind-indentation-offsets-alist
          `((select-clause 0)
            (insert-clause 0)
            (delete-clause 0)
            (update-clause 0)
            (with-clause sqlind-use-anchor-indentation)
            (with-clause-cte-cont 0)
            (statement-continuation 0)
            (select-join-condition 0)
            (select-table 0)
            (select-table-continuation 0)
            (select-join-condition 0)
            (case-clause 0)
            ,@sqlind-default-indentation-offsets-alist))
    (setq sqlind-basic-offset 4))
  (add-hook 'sqlind-minor-mode-hook 'setup-sql-indent))

;; text-mode derivatives
(use-package org
  :if (or (getenv "XDG_DOCUMENTS_DIR") (equal system-type 'darwin))
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :config
  ;; file encryption
  (require 'epa-file)
  (epa-file-enable)

  ;; capture templates
  (let* ((documents-dir-path (cond ((getenv "XDG_DOCUMENTS_DIR") (getenv "XDG_DOCUMENTS_DIR"))
                                   ((equal system-type 'darwin) (expand-file-name "~/Documents"))))
         (journal-path (expand-file-name "journal.gpg" documents-dir-path))
         (meetings-path (expand-file-name "meetings.org" documents-dir-path)))
    (setq org-capture-templates
          `(("m" "Meeting"
             entry (file+headline ,meetings-path "Meetings")
             "* %U %?")))
    (when (member system-name personal-systems)
      (add-to-list 'org-capture-templates
                   `("j" "Journal Entry"
                     entry (file+datetree ,journal-path)
                     "* %<%Y-%m-%d %A %H:%M:%S> - %?" :empty-lines 1))))

  ;; keybinds
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  ;; display settings
  (require 'org-indent)
  (setq org-startup-indented t
        org-pretty-entities t
        org-adapt-indentation t
        org-hide-leading-stars t
        org-hide-emphasis-markers t)

  (defun setup-faces (&optional frame)
    (defun family-available-p (family-name)
      (member family-name (font-family-list)))

    (when (display-graphic-p)
      (let ((selected-family (cond
                              ((family-available-p "DejaVu Sans") "DejaVu Sans"))))
        (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 1.18)
        (set-face-attribute 'org-document-title nil :font "DejaVu Sans" :weight 'bold :height 1.8)
        ;; Resize Org headings
        (dolist (face '((org-level-1 . 1.35)
                        (org-level-2 . 1.3)
                        (org-level-3 . 1.2)
                        (org-level-4 . 1.1)
                        (org-level-5 . 1.1)
                        (org-level-6 . 1.1)
                        (org-level-7 . 1.1)
                        (org-level-8 . 1.1)))
          (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'bold :height (cdr face))))

      ;; use default font for following faces:
      (set-face-attribute 'org-block nil            :foreground 'unspecified :inherit
                          'fixed-pitch              :height 0.85)
      (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
      (set-face-attribute 'org-indent nil           :inherit 'org-hide)
      (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
      (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
                                                               fixed-pitch))
      (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'setup-fonts)
    (setup-fonts)))



;;; appearance
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq column-number-mode t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(defun setup-fonts (&optional frame)
    (defun font-available-p (font-name)
      (find-font (font-spec :name font-name)))

    (let* ((menlo "-*-Menlo-regular-normal-normal-*-11-*-*-*-m-0-iso10646-1")
           (dejavu-mono "DejaVu Sans Mono:pixelsize=13:foundry=PfEd:weight=regular:slant=normal:width=normal:spacing=100:scalable=true")
           (selected-font (cond
                           ((font-available-p menlo) menlo)
                           ((font-available-p dejavu-mono) dejavu-mono))))
      (when (display-graphic-p)
        (set-face-font 'fixed-pitch selected-font))
      (set-frame-font selected-font)))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'setup-fonts)
  (setup-fonts))

(use-package nordic-night-theme
  :ensure t
  :config
  (defun configure-theme (&optional frame)
    (load-theme 'nordic-night t))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'configure-theme)
    (configure-theme)))

(use-package nyan-mode
  :ensure t
  :init
  (setq nyan-minimum-window-length 120
        nyan-wavy-trail t)
  :config
  (when (or (daemonp) (display-graphic-p))
    (nyan-mode 1)
    (nyan-start-animation)))
