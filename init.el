;;; init.el by Shawn Henson -*- lexical-binding: t; -*-

;; To the extent possible under law, the person who associated CC0 with
;; init.el has waived all copyright and related or neighboring rights
;; to init.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; homebrew feature/theme discovery
(let ((elisp-dir (expand-file-name "elisp/" user-emacs-directory))
      (theme-dir (expand-file-name "themes/" user-emacs-directory)))
  (when (file-exists-p elisp-dir)
    (add-to-list 'load-path elisp-dir))
  (when (file-exists-p theme-dir)
    (add-to-list 'load-path theme-dir)
    (setq custom-theme-directory theme-dir)))

;; load common features
(require 'macros)
(require 'package-helper)

(unless (memq epa-file-handler file-name-handler-alist)
    (epa-file-enable))
(defun shenso-ensure-secrets ()
  (unless (boundp 'shenso-secrets-loaded)
    (setq shenso-secrets-loaded nil))
  (unless shenso-secrets-loaded
    (condition-case err
        (load-file (expand-file-name "secrets.el.gpg" user-emacs-directory))
      (error
       (display-warning 'shenso-secrets
                        (format "Could not load secrets: %s"
                                (error-message-string err))
                        :warning
                        "*Warnings*")))))


;;; global config variables
;; general-purpose directories
(set-user-dir
 user-cache-dir     (coalesce (getenv "XDG_CACHE_HOME")
                              ;; TODO: find the appropriate dir for cache 
                              ;;       particularly..... if i ever even bother
                              ;;       to use emacs with windows
                              (when (eq system-type 'windows-nt)
                                (getenv "APPDATA"))
                              "~/.local/cache")
 user-data-dir      (coalesce (getenv "XDG_DATA_HOME")
                              (when (eq system-type 'windows-nt)
                                (getenv "APPDATA"))
                              "~/.local/share")
 user-state-dir     (coalesce (getenv "XDG_STATE_HOME")
                              (when (eq system-type 'windows-nt)
                                (getenv "APPDATA"))
                              "~/.local/state")
 user-documents-dir (coalesce (getenv "XDG_DOCUMENTS_DIR")
                              (when (memq system-type '(darwin windows-nt))
                                "~/Documents")
                              "~/documents")
 user-projects-dir  (cond
                     ((memq system-type '(darwin windows-nt)) "~/Projects")
                     (t "~/projects")))

(setq user-emacs-cache-dir (expand-file-name "emacs/"     user-cache-dir)
      user-emacs-data-dir  (expand-file-name "emacs/"     user-data-dir)
      user-emacs-docs-dir  (expand-file-name "doc/emacs/" user-data-dir)
      user-emacs-state-dir (expand-file-name "emacs/"     user-state-dir))

(make-directory user-emacs-cache-dir :parents)
(make-directory user-emacs-data-dir  :parents)
(make-directory user-emacs-state-dir :parents)

;; specific-purpose directories
(setq user-lsp-bridge-dir (expand-file-name "lsp-bridge" user-emacs-data-dir))
(set-user-dir user-dart-sdk-dir "~/.local/opt/flutter/bin/cache/dart-sdk"
              user-flutter-sdk-dir "~/.local/opt/flutter")

;; system lists
(setq personal-systems '("plato")
      work-systems     '("smith.local"))

;; reset display buffer action alist
(setq display-buffer-alist nil)

(bootstrap-use-package user-emacs-data-dir)


;;; core emacs configurations. any symbols referenced here should be originally
;;; defined by Emacs's C source code, or an emacs lisp file which specifies
;;; "Emacs" as the package.
(use-package emacs
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . hl-line-mode))
  :custom
  (help-window-keep-selected t)
  (read-file-name-completion-ignore-case t)
  (ring-bell-function #'ignore)
  (visible-bell nil)
  :config
  ;; move temp files, autosave, etc out of config dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq auto-save-list-file-prefix
        (expand-file-name "auto-save-list/.saves-" temporary-file-directory))
  ;; try to move eln files to cache dir
  (when (boundp 'native-comp-eln-load-path)
    (let ((new-eln-cache-dir (expand-file-name "eln-cache/"
                                               user-emacs-cache-dir)))
      (make-directory new-eln-cache-dir :parents)
      (setcar native-comp-eln-load-path new-eln-cache-dir)))


  ;; stop custom spam in init file
  (setq disabled-command-function nil
        custom-file (concat user-emacs-directory "custom.el"))
  (ignore-errors
    (load-file custom-file))

  ;; appearance
  (setq column-number-mode t)
  (defun display-max-column-line ()
    (let ((max-col-no
           (if (eq major-mode 'emacs-lisp-mode)
               80
             100)))
      (set-fill-column max-col-no)
      (display-fill-column-indicator-mode)))
  (add-hook 'prog-mode-hook #'display-max-column-line)

  ;; indentation settings
  (setq-default tab-width 4
                indent-tabs-mode nil)

  ;; keybinds
  (with-eval-after-load 'evil
    (evil-define-key 'normal Buffer-menu-mode-map
      (kbd "<return>") #'Buffer-menu-this-window))

  ;; debian puts emacs docs in the non-free repo, which I am NOT addding, so
  ;; thats why this is here.
  (when (file-exists-p user-emacs-docs-dir)
    (add-to-list 'Info-additional-directory-list user-emacs-docs-dir))

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

(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-value (expand-file-name "bookmarks"
                                                 user-emacs-state-dir)))

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  :config
  (defun setup-dired-command ()
    (cond ((or (memq system-type '(gnu gnu/linux gnu/kfreebsd))
               (executable-find "gls"))
           (progn
             (when (executable-find "gls")
               (setq insert-directory-program "gls"))
             (setq dired-listing-switches "-alvF --group-directories-first")
             (setq do-dired-subtree-hack t)))
          ((eq system-type 'darwin)
           ;; -F affects symbolic links on POSIX, but not darwin nor GNU it seems
           (setq dired-listing-switches "-alF")
           ;; -A switch is on BSD, but is not POSIX. I would add other BSDs to
           ;; the predicate, but Darwin appears to be the only supported non-GNU
           ;; BSD.
           (setq do-dired-subtree-hack t))
          ;; womp womp
          (t
           (setq dired-listing-switches "-al"))))
  ;; allow exec-path-to-shell to both run and initialize, but also set up if
  ;; it doesn't load. command changes are purely cosmetic.
  (add-hook 'after-init-hook #'setup-dired-command))

(use-package eshell
  :defer t
  :init
  (setq eshell-directory-name (expand-file-name "eshell" user-emacs-state-dir))
  (make-directory eshell-directory-name :parents)
  (setq eshell-history-file-name (expand-file-name "eshell/history"
                                                   user-emacs-state-dir)))

(use-package tramp
  :defer t
  :init
  (setq tramp-persistency-file-name (expand-file-name "tramp"
                                                      user-emacs-state-dir)))

(use-package url
  :defer t
  :init
  (setq url-configuration-directory (expand-file-name "url/"
                                                      user-emacs-data-dir)))



;;; keyboard bindings
(use-package evil
  :straight t
  :bind (("C-c [" . (lambda ()
                      (interactive)
                      (turn-on-evil-mode)
                      (evil-normal-state)))
         ("C-c ]" . turn-off-evil-mode))
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :hook (after-init . evil-mode)
  :config
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
    :straight t
    :after evil
    :config
    ;; builtin mode maps
    (evil-collection-init
     '(calendar
       custom
       debug
       diff-mode
       dired
       edebug
       eshell
       grep
       help
       image-dired
       info
       wdired))

    ;; override dired change
    (evil-define-key 'normal dired-mode-map "q" (lambda ()
                                                  (interactive)
                                                  (quit-window t)))

    ;; package mode maps
    (eval-after-load 'ement #'evil-collection-ement-setup)
    (eval-after-load 'ivy #'evil-collection-ivy-setup)
    (eval-after-load 'magit #'evil-collection-magit-setup))

(use-package evil-dired
  :after evil
  :config
  (evil-dired-setup)
  (unless (featurep 'evil-collection)
    (eval-after-load 'evil-collection #'evil-dired-setup)))

(use-package evil-org
    :straight t
    :after (evil org)
    :config
    (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    ;; we need to add the hook after setting org-agenda keys, not in :hook
    (add-hook 'org-mode #'evil-org-mode))

(use-package evil-vterm ; homemade with love :)
  :after (evil vterm)
  :config
  (evil-vterm-init))

(use-package free-keys
  :straight '(free-keys :type git :host github :repo "Fuco1/free-keys"))



;;; integrations
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
  :after vterm
  :bind (:map vterm-command-map
              ("a" . toggle-vterm-anchor))
  :defer nil
  :config
  (anchor-vterm-to-bottom))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package magit
  :straight t
  :defer t
  :init
  (setq transient-history-file (expand-file-name "transient/history.el"
                                                 user-emacs-state-dir)
        transient-levels-file (expand-file-name "transient/levels.el"
                                                user-emacs-state-dir)
        transient-values-file (expand-file-name "transient/values.el"
                                                user-emacs-state-dir))
  :config
  ;; display git status buffer in current window
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                           (not (memq (with-current-buffer buffer major-mode)
                                      '(magit-process-mode
                                        magit-revision-mode
                                        magit-diff-mode
                                        magit-stash-mode
                                        magit-status-mode))))
                      '(display-buffer-same-window)
                    (cond ((eq (with-current-buffer buffer major-mode)
                               'magit-status-mode)
                           '(display-buffer-same-window))
                          (t nil)))))))

(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver"
                              "multiserver" "resources")
            :build (:not compile))
  :after (markdown-mode yasnippet)
  :if (file-exists-p user-lsp-bridge-dir)
  :init
  (defun enable-lsp-bridge-if-local-file ()
    (unless (file-remote-p default-directory)
      (lsp-bridge-mode)))
  :hook ((elisp-mode      . enable-lsp-bridge-if-local-file)
         (python-mode     . enable-lsp-bridge-if-local-file)
         (go-mode         . enable-lsp-bridge-if-local-file)
         (dart-mode       . enable-lsp-bridge-if-local-file)
         (typescript-mode . enable-lsp-bridge-if-local-file))
  :defer nil
  :custom
  (acm-enable-copilot nil)
  (lsp-bridge-enable-hover-diagnostic t)
  :config
  (setq lsp-bridge-enable-with-tramp nil)

  (setq lsp-bridge-python-command
        (expand-file-name ".venv/bin/python" user-lsp-bridge-dir))
  (require 'acm-backend-elisp)
  (global-lsp-bridge-mode)

  (defvar-keymap lsp-bridge-command-map
    "d"   #'lsp-bridge-find-def
    "r"   #'lsp-bridge-find-references
    "i"   #'lsp-bridge-show-documentation
    "l"   #'lsp-bridge-popup-complete-menu
    "C-l" #'lsp-bridge-popup-complete-menu)
  ;; do we really need the lasso?
  (keymap-set help-map (kbd "l") lsp-bridge-command-map)
  (with-eval-after-load 'evil
    ;; zz does this anyhow
    (evil-global-set-key 'normal (kbd "C-l") lsp-bridge-command-map)
    (evil-global-set-key 'insert (kbd "C-l") lsp-bridge-command-map))

  (add-to-list 'display-buffer-alist
               '("\\*lsp-bridge-doc\\*"
                 (display-buffer-use-least-recent-window)))

  ;; when the theme changes the autocomplete menu uses the old background color.
  ;; this lambda forcefully resets the face colors.
  (with-eval-after-load 'theme-timer
    (add-hook 'theme-timer-change-hook (lambda ()
                                         (acm-frame-init-colors t)))))

(use-package dape
  :straight t
  :init
  (setq dape-prefix-key "\C-x\C-a")
  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  (setq dape-cwd-function 'projectile-project-root))

(use-package ement
  :straight t
  :commands ement-connect)

(use-package gptel
  :straight t
  :defer t
  :config
  (setq gptel-directives
        `((default . ,(concat
                       "You are a large language model living inside a GNU Emacs"
                       " Buffer. Like all features in GNU Emacs you are designed"
                       " to be helpful either with computer programming in"
                       " general, programming in Emacs Lisp, or navigating,"
                       " using, or understanding various aspects of GNU Emacs."
                       ))))
  (shenso-ensure-secrets)
  (when (fboundp 'shenso-secrets-claude-api-key)
        (setq gptel-model 'claude-3-7-sonnet-20250219)
        (setq gptel-backend
              (gptel-make-anthropic "Claude"
                :stream t
                :key 'shenso-secrets-claude-api-key))))



;;; navigation/buffer completion
(use-package dired-subtree
  :straight t)

(use-package dired-subtree-hack
  :after dired-subtree
  :if (or (eq system-type 'gnu/linux) (executable-find "gls")))

(use-package ivy
  :straight t
  :config
  (ivy-mode))

(use-package projectile
  :straight t
  :bind-keymap (("C-c p" . projectile-command-map)
                ("s-p"   . projectile-command-map))
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :init
  (setq project-mode-line "Projectile") ; avoid tramp overhead
  :config
  (setq projectile-key
        (cond ((eq system-type 'darwin) (kbd "s-p"))
              (t                        (kbd "C-c p"))))
  (setq projectile-project-search-path (list user-projects-dir)
        projectile-cache-file          (expand-file-name
                                        "projectile.cache"
                                        user-emacs-state-dir)
        projectile-known-projects-file (expand-file-name
                                        "projectile-known-projects.eld"
                                        user-emacs-state-dir))

  ;; avoid activity on tramp
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory)
      ad-do-it))

  ;; ignore packages with git directories
  (setq shenso-ignore-project-paths
        `(,(expand-file-name "straight/" user-emacs-data-dir)))
  (defun shenso-ignore-project-p (file)
    (if (file-remote-p file)
        t
      (progn
        (let* ((abs-file-name (expand-file-name file))
               (file-parts (file-name-split abs-file-name))
               (num-file-parts (length file-parts))
               (ignore-path-found nil)
               (tail shenso-ignore-project-paths))
          (while (and tail (not ignore-path-found))
            (let* ((v (car tail))
                   (path-parts (-butlast (file-name-split v)))
                   (num-path-parts (length path-parts)))
              (unless (> num-path-parts num-file-parts)
                (setq ignore-path-found
                      (equal path-parts (take num-path-parts file-parts))))
              (setq tail (cdr tail))))
          ignore-path-found))))
    (setq projectile-ignored-project-function #'shenso-ignore-project-p)

  (projectile-global-mode)
  (projectile-discover-projects-in-search-path)
  (define-key projectile-mode-map projectile-key 'projectile-command-map))

(use-package shenso-windowing
  :config
  (setq split-window-preferred-function #'split-window-insensibly))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode))


;;; major modes

;; prog-mode derivatives
(use-package cc-mode
  :defer t
  :init
  (setq-default c-default-style
                '((c-mode . "k&r")
                  (c++-mode . "k&r")
                  (csharp-mode . "bsd")
                  (other . "java")))
  (setq-default c-basic-offset 4))

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

;; this sucks and has insane defaults, but it sucks less than default sql-mode
(use-package sql-indent
  :straight t
  :after sql-mode
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
  (add-hook 'sqlind-minor-mode-hook #'setup-sql-indent))

;; text-mode derivatives
(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" "\\.tsv\\'")
  :hook ((csv-mode tsv-mode) . csv-align-mode))

(use-package csv-rainbow
  :after csv-mode)

(use-package markdown-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :config
  (add-hook 'yaml-mode-hook #'display-line-numbers-mode))

(use-package org
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  ;; capture templates
  (setq org-capture-templates nil)
  (when user-documents-dir
    (when (member system-name work-systems)
      (add-to-list 'org-capture-templates
            `("m" "Meeting"
               entry (file+headline
                      ,(expand-file-name "meetings.org" user-documents-dir)
                      "Meetings")
               "* %U %?")))
    (when (member system-name personal-systems)
      (add-to-list 'org-capture-templates
                   `("j" "Journal Entry"
                     entry (file+olp+datetree
                            ,(expand-file-name "journal.gpg" user-documents-dir))
                     "* %<%Y-%m-%d %A %H:%M:%S> - %?" :empty-lines 1)))))



;;; appearance
(use-package nordic-night-theme
  :straight t)

(use-package modus-themes
  :straight t
  :if (< emacs-major-version 30))

(use-package theme-timer
  :after nordic-night-theme
  :custom
  (theme-timer-day-time-theme 'modus-operandi-tritanopia)
  (theme-timer-night-time-theme 'nordic-night)
  :config
  (call-on-client-frame-init theme-timer-init))

(use-package shenso-font-theme
  :config
  (load-theme 'shenso-font t))

(use-package org-pretty-theme ; homemade with love :)
  :after theme-timer
  :config
  (add-hook 'theme-timer-change-hook #'reload-org-pretty))

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
  :init
  (setq nyan-minimum-window-length 120
        nyan-wavy-trail t)
  :config
  (defun setup-nyan-mode ()
    (if (memq theme-timer-night-time-theme custom-enabled-themes)
        (progn
          (nyan-mode 1)
          (nyan-start-animation))
      (nyan-mode -1)))
  (setup-nyan-mode)
  (add-hook 'theme-timer-change-hook #'setup-nyan-mode))

(use-package diminish
  :straight t
  :config
  (let ((target-modes '((projectile      . projectile-mode)
                        (yasnippet       . yas-minor-mode)
                        (ivy             . ivy-mode)
                        (evil-collection . evil-collection-unimpaired-mode))))
    (dolist (target-mode target-modes)
      (with-eval-after-load (car target-mode)
        (diminish (cdr target-mode))))))
