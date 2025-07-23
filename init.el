;;; init.el --- Shawn Henson's personal GNU Emacs config -*- lexical-binding: t; -*-

;; To the extent possible under law, the person who associated CC0 with
;; init.el has waived all copyright and related or neighboring rights
;; to init.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:

;;; Code:

;;;; set-up load path and initial requirements:
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes/" user-emacs-directory))
(setq custom-theme-directory (expand-file-name "themes/" user-emacs-directory))

;; personal libraries:
(require 'package-helper)
(require 'shenso-macros)
(require 'shenso-secrets)
;; built-in libraries:
(require 'help)
(require 'info)

;;;; Customizations:
(defgroup shenso ()
  "My personal customization options."
  :group 'local
  :prefix "shenso-")

(defgroup shenso-directories ()
  "Directory customizations."
  :group 'shenso)

(defgroup shenso-user-directories ()
  "Shared directories for my local user.  E.g. cache, config, data, etc."
  :group 'shenso-directories)

(defgroup shenso-emacs-directories ()
  "Directories used by GNU Emacs."
  :group 'shenso-directories)

(defgroup shenso-emacs-package-directories ()
  "Directories used by GNU Emacs packages."
  :group 'shenso-emacs-directories)

(defgroup shenso-misc-directories ()
  "Directories of non-Emacs programs and dependencies."
  :group 'shenso)

(defgroup shenso-systems ()
  "System classifications."
  :group 'shenso)

;;; Custom user directories:
(defcustom user-cache-dir
  (abs-dir (or (getenv "XDG_CACHE_HOME")
               (and (eq system-type 'windows-nt) (getenv "APPDATA"))
               "~/.local/cache"))
  "User cache directory."
  :type 'directory
  :group 'shenso-user-directories)

(defcustom user-data-dir
  (abs-dir (or (getenv "XDG_DATA_HOME")
               (if (eq system-type 'windows-nt)
                 (getenv "APPDATA"))
               "~/.local/share"))
  "User data directory."
  :type 'directory
  :group 'shenso-user-directories)

(defcustom user-state-dir
  (abs-dir (or (getenv "XDG_STATE_HOME")
               (if (eq system-type 'windows-nt)
                 (getenv "APPDATA"))
               "~/.local/state"))
  "User state directory."
  :type 'directory
  :group 'shenso-user-directories)

(defcustom user-documents-dir
  (any-abs-dir (getenv "XDG_DOCUMENTS_DIR")
               "~/documents"
               "~/Documents")
  "User documents directory."
  :type 'directory
  :group 'shenso-user-directories)

(defcustom user-projects-dir
  (any-abs-dir "~/projects"
               "~/Projects")
  "User projects directory."
  :type 'directory
  :group 'shenso-user-directories)

;;; Custom Emacs directories
(defcustom user-emacs-cache-dir
  (expand-file-name "emacs/" user-cache-dir)
  "Emacs cache directory."
  :type 'directory
  :group 'shenso-emacs-directories)

(defcustom user-emacs-data-dir
  (expand-file-name "emacs/" user-data-dir)
  "Emacs data directory."
  :type 'directory
  :group 'shenso-emacs-directories)

(defcustom user-emacs-docs-dir
  (expand-file-name "emacs/docs" user-data-dir)
  "Emacs documentation directory."
  :type 'directory
  :group 'shenso-emacs-directories)

(defcustom user-emacs-state-dir
  (expand-file-name "emacs" user-state-dir)
  "Emacs state directory."
  :type 'directory
  :group 'shenso-emacs-directories)

;;; Custom Emacs package directories:
(defcustom user-lsp-bridge-venv-dir
  (expand-file-name "lsp-bridge/.venv" user-emacs-data-dir)
  "Directory of the python virtual environment for lsp-bridge."
  :type 'directory
  :group 'shenso-emacs-package-directories)

;;; Dependency installation directories:
(defcustom user-dart-sdk-dir
  (expand-file-name "~/.local/opt/flutter/bin/cache/dart-sdk")
  "Installation directory of the Dart SDK."
  :type 'directory
  :group 'shenso-misc-directories)

(defcustom user-flutter-sdk-dir
  (expand-file-name "~/.local/opt/flutter")
  "Installation directory of the Dart SDK."
  :type 'directory
  :group 'shenso-misc-directories)

;;; Custom system classifications:
(defcustom personal-systems
  '("plato" "aristotle.local")
  "Systems used for personal computing."
  :type '(set string)
  :group 'shenso-systems)

(defcustom work-systems
  '("smith.local")
  "Systems owned by an employer, or used solely for business purposes."
  :type '(set string)
  :group 'shenso-systems)

(defcustom school-systems
  '("aristotle.local")
  "Systems used for school: lecture notes, homework, etc."
  :type '(set string)
  :group 'shenso-systems)

(make-directory user-emacs-cache-dir :parents)
(make-directory user-emacs-data-dir  :parents)
(make-directory user-emacs-state-dir :parents)

;; reset display buffer action alist
(setq display-buffer-alist nil)

(bootstrap-use-package user-emacs-data-dir)

;;; Helper functions, no external requirements:
(defun display-max-column-line ()
  "Display column line.  Max 100 typically, 80 for elisp."
  (let ((max-col-no
         (if (eq major-mode 'emacs-lisp-mode)
             80
           100)))
    (set-fill-column max-col-no)
    (display-fill-column-indicator-mode)))

(define-inline work-system-p ()
  (inline-quote (member (system-name) work-systems)))

(define-inline personal-system-p ()
  (inline-quote (member (system-name) personal-systems)))

(define-inline school-system-p ()
  (inline-quote (member (system-name) school-systems)))

;;;; Package configurations:

;;; core emacs configurations. any symbols referenced here should be originally
;;; defined by Emacs's C source code, or an emacs lisp file which specifies
;;; "Emacs" as the package.
(use-package emacs
  :straight nil
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . hl-line-mode))
  :custom
  ;; user information:
  (user-full-name    "Shawn Henson")
  (user-mail-address "shawn@shenso.name")

  ;; trusted content:
  (trusted-content (list
                    ;; `trusted-content-p' only looks at relative paths for some
                    ;; reason
                    (string-replace (expand-file-name "~") "~"
                                    (expand-file-name "elisp/" user-emacs-directory))
                    (string-replace (expand-file-name "~") "~"
                                    (expand-file-name "themes/" user-emacs-directory))
                    (string-replace (expand-file-name "~") "~" user-projects-dir)))

  ;; directories:
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-"
                                                temporary-file-directory))
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (Info-additional-directory-list (list user-emacs-docs-dir))

  ;; misc
  (help-window-keep-selected t)
  (read-file-name-completion-ignore-case t)
  (ring-bell-function #'ignore)
  (visible-bell nil)
  (disabled-command-function nil)
  (column-number-mode t)
  (tab-width 4)
  (indent-tabs-mode nil)

  :config
  ;; try to move eln files to cache dir
  (when (boundp 'native-comp-eln-load-path)
    (let ((new-eln-cache-dir (expand-file-name "eln-cache/"
                                               user-emacs-cache-dir)))
      (make-directory new-eln-cache-dir :parents)
      (setcar native-comp-eln-load-path new-eln-cache-dir)))

  (ignore-errors
    (load-file custom-file))

  ;; appearance
  (add-hook 'prog-mode-hook #'display-max-column-line)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs took %s seconds to start!" (emacs-init-time))))
  (when (daemonp)
    ;; ensure daemon clients still get to see the glorious emacs gnu
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (when (and (string= (buffer-name) "*scratch*")
                           (not (buffer-file-name)))
                  (display-about-screen))))))

(use-package ns-win
  :straight nil
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super))

(use-package bookmark
  :straight nil :defer t
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" user-emacs-state-dir)))

(use-package dired
  :straight nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (defun dired-toggle-show-hidden-files ()
    (interactive)
    (with-current-buffer (current-buffer)
      (let ((switches-split (string-split dired-actual-switches)))
        (if (member "-a" switches-split)
            (setq switches-split (delete "-a" switches-split))
          (push "-a" switches-split))
        (setq dired-actual-switches (string-join switches-split " "))
        (revert-buffer))))

  (when (executable-find "gls")
    (setq insert-directory-program "gls"))

  ;; allow exec-path-to-shell to both run and initialize, but also set up if
  ;; it doesn't load. command changes are purely cosmetic.
  (add-hook 'after-init-hook
            (lambda ()
              (cond ((or (memq system-type '(gnu gnu/linux gnu/kfreebsd))
                         (equal insert-directory-program "gls"))
                     (setq dired-listing-switches "-a -lvF --group-directories-first"))
                    ((eq system-type 'darwin)
                     ;; -F affects symbolic links on POSIX, but not darwin nor GNU it seems
                     (setq dired-listing-switches "-a -lF"))
                    ;; womp womp
                    (t
                     (setq dired-listing-switches "-a -l"))))))

(use-package esh-mode
  :straight nil :defer t
  :custom
  (eshell-directory-name (expand-file-name "eshell" user-emacs-state-dir))
  :config
  (make-directory eshell-directory-name :parents))

(use-package em-hist
  :straight nil :defer t
  :custom
  (eshell-history-file-name (expand-file-name "eshell/history" user-emacs-state-dir))
  :config
  (make-directory eshell-history-file-name :parents))

(use-package ibuffer
  :straight nil
  :bind ("C-x C-b" . ibuffer))

(use-package ielm
  :straight nil :defer t
  :custom
  (ielm-history-file-name (expand-file-name "ielm-history.eld" user-emacs-state-dir)))

(use-package ido
  :straight nil :defer t
  :custom
  (ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-state-dir)))

(use-package nsm
  :straight nil :defer t
  :custom
  (nsm-settings-file (expand-file-name "network-security.data" user-emacs-data-dir)))

(use-package tramp
  :straight nil :defer t
  :custom
  (tramp-persistency-file-name (expand-file-name "tramp" user-emacs-state-dir)))

(use-package url
  :straight nil :defer t
  :custom
  (url-configuration-directory (expand-file-name "url/" user-emacs-data-dir)))



;;; keyboard bindings:
(use-package evil
  :straight t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :hook (after-init . evil-mode)
  :config
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
    :straight t
    :after evil
    :config
    ;; builtin mode maps
    (evil-collection-init
     '(bookmark calendar custom debug diff-mode dired edebug eldoc eshell eww
                gnus grep help ibuffer image-dired imenu info man org pdf tetris
                wdired xref))

    ;; package mode maps
    (eval-after-load 'ement #'evil-collection-ement-setup)
    (eval-after-load 'free-keys #'evil-collection-free-keys-setup)
    (eval-after-load 'ivy #'evil-collection-ivy-setup)
    (eval-after-load 'magit #'evil-collection-magit-setup))

(use-package shenso-evil
  :straight nil
  :after evil
  :config
  (shenso-evil-init)
  (unless (featurep 'evil-collection)
    (eval-after-load 'evil-collection #'shenso-evil-dired-setup)))

(use-package evil-org
    :straight t
    :after (evil org)
    :config
    (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    ;; we need to add the hook after setting org-agenda keys, not in :hook
    (add-hook 'org-mode #'evil-org-mode))

(use-package free-keys
  :straight '(free-keys :type git :host github :repo "Fuco1/free-keys"))



;;; integrations:
(use-package vterm
  :straight t
  :bind-keymap ("C-x v" . vterm-command-map)
  :init
  (define-key ctl-x-map "v" nil) ; we use magit anyways
  :config
  (defun create-new-vterm (name)
    (vterm (generate-new-buffer-name name)))
  (defun create-new-unnamed-vterm ()
    (interactive)
    (create-new-vterm "*secondary vterm*"))
  (defun create-new-named-vterm (name)
    (interactive "sSession name: ")
    (create-new-vterm (concat "*vterm: " name "*")))

  (defvar-keymap vterm-command-map
    "t" #'vterm
    "m" #'create-new-unnamed-vterm ; saves me from having to do C-u <n> C-x vt
    "n" #'create-new-named-vterm)
  (define-key ctl-x-map "v" 'vterm-command-map))

(use-package vterm-anchor ; homemade with love :)
  :straight nil
  :after vterm
  :bind (:map vterm-command-map
              ("a" . toggle-vterm-anchor))
  :defer nil
  :config
  (anchor-vterm-to-bottom))

(use-package exec-path-from-shell
  :straight t
  :if (or (daemonp) (window-system))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package magit
  :straight t
  :defer t
  :init
  (defun shenso-magit-display-buffer (buffer)
    (let ((blacklist '(magit-process-mode
                       magit-revision-mode
                       magit-diff-mode
                       magit-stash-mode))
          (whitelist '(magit-status-mode))
          (buffer-mode (with-current-buffer buffer major-mode)))
      (display-buffer
       buffer
       (if (or (and (derived-mode-p 'magit-mode)
                    (not (memq buffer-mode blacklist)))
               (memq buffer-mode whitelist))
           '(display-buffer-same-window)))))
  :custom
  (transient-history-file (expand-file-name "transient/history.el"
                                            user-emacs-state-dir))
  (transient-levels-file (expand-file-name "transient/levels.el"
                                           user-emacs-state-dir))
  (transient-values-file (expand-file-name "transient/values.el"
                                           user-emacs-state-dir))
  (magit-display-buffer-function #'shenso-magit-display-buffer))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver"
                              "multiserver" "resources")
            :build (:not compile))
  :after (markdown-mode yasnippet)
  :if (and (file-exists-p user-lsp-bridge-venv-dir)
           (file-exists-p (expand-file-name "bin/python" user-lsp-bridge-venv-dir))
           (or (daemonp)
               (window-system)
               ;; should be available in Emacs 31
               (featurep 'tty-child-frames)))
  :init
  (defun enable-lsp-bridge-if-local-file ()
    (unless (file-remote-p default-directory)
      (lsp-bridge-mode)))
  (defun shenso-lsp-bridge-find-references ()
    (interactive)
    (cond ((eq major-mode 'emacs-lisp-mode)
           (condition-case nil
               (xref-find-references (thing-at-point 'symbol))
             (error (message "No references found at point."))))
          (t
           (lsp-bridge-find-references))))
  :bind (:map lsp-bridge-mode-map
              ("C-l d"   . lsp-bridge-find-def)
              ("C-l r"   . shenso-lsp-bridge-find-references)
              ("C-l i"   . lsp-bridge-show-documentation)
              ("C-l l"   . lsp-bridge-popup-complete-menu)
              ("C-l C-l" . lsp-bridge-popup-complete-menu))

  :hook ((emacs-lisp-mode      . enable-lsp-bridge-if-local-file)
         (python-mode          . enable-lsp-bridge-if-local-file)
         (go-mode              . enable-lsp-bridge-if-local-file)
         (dart-mode            . enable-lsp-bridge-if-local-file)
         (typescript-mode      . enable-lsp-bridge-if-local-file)
         (c-mode               . enable-lsp-bridge-if-local-file))
  :custom
  (acm-enable-copilot nil)
  (lsp-bridge-enable-hover-diagnostic t)
  (lsp-bridge-c-lsp-server "clangd")
  (lsp-bridge-python-command (expand-file-name "bin/python"
                                               user-lsp-bridge-venv-dir))

  :config
  (require 'acm-backend-elisp)
  (setq lsp-bridge-enable-with-tramp nil)
  (add-to-list 'display-buffer-alist
               '("\\*lsp-bridge-doc\\*"
                 (display-buffer-use-least-recent-window)))
  ;; when the theme changes the autocomplete menu uses the old background color.
  ;; this lambda forcefully resets the face colors.
  (with-eval-after-load 'theme-timer
    (add-hook 'theme-timer-change-hook (lambda ()
                                         (acm-frame-init-colors t)))))

(use-package flymake
  :straight nil
  :hook (emacs-lisp-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-p" . flymake-goto-prev-error)
              ("M-n" . flymake-goto-next-error)))

(use-package ement
  :straight t
  :commands ement-connect)

(use-package gnus
  :straight nil
  :custom
  (gnus-select-method '(nnnil nil))
  (gnus-secondary-select-methods '((nnml "")
                                   (nnimap "imappro.zoho.com"
                                           (nnimap-address "imappro.zoho.com")
                                           (nnimap-server-port "993")
                                           (nnimap-stream ssl))))
  (gnus-posting-styles '(("imappro.zoho.com"
                          ("X-Message-SMTP-Method"
                           "smtp smtppro.zoho.com 587 shawn@shenso.name"))))

  ;; move crap out of my home directory!!!!
  (gnus-home-directory (expand-file-name "gnus" user-emacs-data-dir))
  (gnus-directory (expand-file-name "gnus/news" user-emacs-data-dir))
  (gnus-kill-files-directory (expand-file-name "gnus/news" user-emacs-data-dir))
  (gnus-article-save-directory (expand-file-name "gnus/news" user-emacs-data-dir))
  (gnus-cache-directory (expand-file-name "gnus" user-emacs-cache-dir))
  (message-directory (expand-file-name "mail" user-emacs-data-dir))
  (nnfolder-directory (expand-file-name "mail" user-emacs-data-dir))
  (nnfolder-active-file (expand-file-name "mail/active" user-emacs-data-dir))
  (nnfolder-newsgroups-file (expand-file-name "mail/newsgroups" user-emacs-data-dir)))

(use-package smtpmail
  :straight nil
  :custom
  (smtpmail-smtp-user                        "shawn@shenso.name")
  (smtpmail-default-smtp-server              "smtppro.zoho.com")
  (smtpmail-smtp-server                      "smtppro.zoho.com")
  (smtpmail-smtp-service                     587)
  (smtpmail-servers-requiring-authorization  "smtppro.zoho.com")
  (smtpmail-stream-type                      'starttls)
  (smtpmail-use-gnutls                       t)
  (smtpmail-debug-info                       t))

(use-package gptel
  :straight t
  :defer t
  :custom
  (gptel-directives
   (or
    (if (file-exists-p (expand-file-name "gptel-prompts.json"
                                         user-emacs-directory))
        (with-temp-buffer
          (insert-file-contents (expand-file-name "gptel-prompts.json"
                                                  user-emacs-directory))
          (let ((data (json-parse-buffer))
                (directives nil))
            (maphash (lambda (key val)
                       (push (cons (intern key) (string-join val "\n")) directives))
                     data)
            directives)))
    nil))
  :config
  (when (personal-system-p)
      (setq gptel-model 'claude-3-7-sonnet-20250219)
      (setq gptel-backend
            (gptel-make-anthropic "Claude"
              :stream t
              :protocol "https"
              :key 'gptel-api-key))))



;;; navigation/buffer completion:
(use-package dired-subtree
  :straight t
  :after dired)

(use-package dired-subtree-hack
  :straight nil
  :after dired-subtree
  ;; -A switch is on BSD, but is not POSIX. I would add other BSDs to
  ;; the predicate, but Darwin appears to be the only supported non-GNU
  ;; BSD.
  :if (or (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin))
          (executable-find "gls"))
  :config
  ;; allow exec-path-to-shell to both run and initialize, but also set up if
  ;; it doesn't load. command changes are purely cosmetic.
  (add-hook 'after-init-hook #'install-dired-subtree-hack))

(use-package ivy
  :straight t
  :config
  (ivy-mode))

(use-package project
  :straight nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :custom
  (project-list-file (expand-file-name "projects" user-emacs-state-dir))
  (project-mode-line "Project"))

(use-package shenso-windowing
  :straight nil
  :custom
  (split-window-preferred-function #'split-window-insensibly))

(use-package yasnippet
  :straight t
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-data-dir)))
  :config
  (yas-global-mode))


;;; major modes:

;; prog-mode derivatives
(use-package cc-mode
  :defer t
  :custom
  (c-default-style '((c-mode      . "k&r")
                     (c++-mode    . "k&r")
                     (csharp-mode . "bsd")
                     (other       . "java")))
  (c-basic-offset 4))

(use-package dart-mode
  :straight t
  :mode ("\\.dart\\'"))

(use-package flutter
    :straight t
    :if (file-exists-p user-flutter-sdk-dir)
    :after dart-mode
    :init
    (setq flutter-sdk-path user-flutter-sdk-dir))

(use-package go-mode
  :straight t
  :mode ("\\.go\\'")
  :hook (go-mode . (lambda ()
                     (add-hook 'before-save-hook #'gofmt-before-save nil t))))

(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'"))

(use-package bigquery
  :straight '(bigquery :type git :host github :repo "shenso/bigquery.el")
  :if (member (system-name) (cons "plato" work-systems))
  :config
  (when (member (system-name) work-systems)
    (add-to-list 'auto-mode-alist '("\\.sql\\'" . googlesql-mode))))

(use-package sql-indent
  :straight t
  :after sql
  :config
  (setq-default sqlind-indentation-offsets-alist
                `((select-clause 0)
                  (insert-clause 0)
                  (delete-clause 0)
                  (update-clause 0)
                  ,@sqlind-default-indentation-offsets-alist)))

;; text-mode derivatives
(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" "\\.tsv\\'")
  :hook ((csv-mode tsv-mode) . csv-align-mode))

(use-package csv-rainbow
  :straight nil
  :defer t
  :after csv-mode)

(use-package markdown-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :hook (yaml-mode-hook . display-line-numbers-mode))

(use-package org
  :straight nil
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :init
  (defun build-course-capture-template (course-path index)
    (list (concat "c" (int-to-string index))
          (concat (file-name-base course-path) " Lecture Notes")
          'entry
          (list 'file+olp+datetree
                (expand-file-name "lectures.org" course-path))
          "* %<%Y-%m-%d> - %?"
          :tree-type 'month
          :empty-lines 1))

  (defun build-course-capture-templates ()
    (let* ((courses-path (expand-file-name "Courses/Active/" user-documents-dir))
           (files (ignore-error file-missing
                    (directory-files courses-path
                                     nil
                                     directory-files-no-dot-files-regexp)))
           (file-paths (mapcar
                        (lambda (file)
                          (expand-file-name file courses-path))
                        files)))
      (append
       (list (list "c" "Course Lecture Notes"))
       (seq-map-indexed #'build-course-capture-template
                        (seq-filter #'file-directory-p file-paths)))))
  :custom
  (org-capture-templates
   `(,@(if (work-system-p)
           `(("m" "Meeting" entry
              (file+headline ,(expand-file-name "meetings.org"
                                                user-documents-dir)
                             "Meetings")
              "* %U %?")))
     ,@(if (personal-system-p)
           `(("j" "Journal Entry" entry
              (file+olp+datetree ,(expand-file-name "journal.gpg"
                                                    user-documents-dir)
                                 "* %<%Y-%m-%d %A %H:%M:%S> - %?"
                                 ::empty-lines 1))))
     ,@(if (school-system-p)
           (build-course-capture-templates))))
  (org-blank-before-new-entry '((heading . auto)
                                (plain-list-item . nil)))
  (org-cycle-separator-lines 1))



;;; appearance:
(use-package nordic-night-theme
  :straight t
  :config
  (when (daemonp)
    (let (load-nordic-theme)
      (setq load-nordic-theme
            (lambda ()
              (when (nordic-night--fullcolorp)
                (load-theme 'nordic-night t)
                (when (featurep 'theme-timer)
                  (theme-timer-use-time-appropriate-theme))
                (remove-hook 'server-after-make-frame-hook load-nordic-theme))))
      (add-hook 'server-after-make-frame-hook load-nordic-theme))))

(use-package modus-themes
  :straight t
  :if (< emacs-major-version 30))

(use-package theme-timer
  :straight nil
  :after nordic-night-theme
  :custom
  (theme-timer-day-time-theme 'modus-operandi-tritanopia)
  (theme-timer-night-time-theme 'nordic-night)
  :config
  (call-on-client-frame-init #'theme-timer-init))

(use-package shenso-font-theme
  :straight nil
  :config
  (call-on-client-frame-init #'reload-shenso-font))

(use-package org-pretty-theme ; homemade with love :)
  :straight nil
  :defer nil
  :after org
  :config
  (load-theme 'org-pretty t)
  (with-eval-after-load 'theme-timer
    (add-hook 'theme-timer-change-hook #'org-pretty-reload)))

(use-package all-the-icons
  :straight t
  :if (or (display-graphic-p) (daemonp)))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :if (or (display-graphic-p) (daemonp))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nyan-mode
  :straight t
  :after theme-timer
  :custom
  (nyan-minimum-window-width 64)
  (nyan-wavy-trail t)
  :config
  (defun maybe-toggle-nyan-mode ()
    (if (memq theme-timer-night-time-theme custom-enabled-themes)
        (progn
          (nyan-mode 1)
          (nyan-start-animation))
      (nyan-mode -1)))
  (maybe-toggle-nyan-mode)
  (add-hook 'theme-timer-change-hook #'maybe-toggle-nyan-mode))

(use-package diminish
  :straight t
  :config
  (let ((target-modes '((yasnippet       . yas-minor-mode)
                        (ivy             . ivy-mode)
                        (evil-collection . evil-collection-unimpaired-mode))))
    (dolist (target-mode target-modes)
      (with-eval-after-load (car target-mode)
        (diminish (cdr target-mode))))))

;;; stuff im working on:
(defvar asyncio-dir (expand-file-name "asyncio.el/" user-projects-dir))
(use-package asyncio
  :straight nil
  :if (file-exists-p asyncio-dir)
  :load-path asyncio-dir)

;;; init.el ends here
