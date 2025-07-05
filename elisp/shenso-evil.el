;;; shenso-evil.el --- My personal evil bindings -*- lexical-binding: t; -*-

;; To the extent possible under law, the person who associated CC0 with
;; init.el has waived all copyright and related or neighboring rights
;; to init.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:

;;; Code:

(require 'evil)

(defun shenso-evil-init ()
  (shenso-evil-buffer-menu-setup)
  (shenso-evil-dired-setup)
  (shenso-evil-man-setup)

  ;; package enhancements:
  (eval-after-load 'lsp-bridge #'shenso-evil-lsp-bridge-setup)
  (eval-after-load 'vterm #'shenso-evil-vterm-setup))

;; Setup functions:

(defun shenso-evil-buffer-menu-setup ()
  (interactive)
  (evil-define-key 'normal Buffer-menu-mode-map
    "RET" #'Buffer-menu-this-window))

(defun shenso-evil-dired-setup ()
  (interactive)
  (evil-define-key 'normal dired-mode-map
    "f" #'find-name-dired
    "p" #'find-lisp-find-dired
    "P" #'find-lisp-find-dired-other-window
    "N" #'find-lisp-find-dired-filter
    "F" #'find-grep-dired
    "u" #'dired-up-directory
    "q" #'quit-window
    ;;"n" #'evil-search-next
    ))

(defun shenso-evil-man-setup ()
  (interactive)
  (evil-define-key 'normal Man-mode-map
    "RET" #'man-follow))

(defun shenso-evil-lsp-bridge-setup ()
  (interactive)
  (evil-define-key 'normal lsp-bridge-ref-mode-map
    "0"   #'lsp-bridge-ref-beginning-of-line
    "RET" #'lsp-bridge-ref-open-file-and-stay
    "SPC" #'lsp-bridge-ref-open-file
    "D"   #'lsp-bridge-ref-remove-line-from-results
    "f"   #'lsp-bridge-ref-jump-next-file
    "h"   #'lsp-bridge-ref-jump-prev-file
    "j"   #'lsp-bridge-ref-jump-next-keyword
    "k"   #'lsp-bridge-ref-jump-prev-keyword
    "q"   #'lsp-bridge-ref-quit))

(defun shenso-evil-vterm-setup ()
  (interactive)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (add-hook 'buffer-list-update-hook #'shenso-evil--on-window-select)
  (add-hook 'window-selection-change-functions
            #'shenso-evil--on-window-select))

;; Helpers:

(defun shenso-evil--on-window-select (&optional window)
  (when (eq major-mode 'vterm-mode)
    (call-interactively #'evil-emacs-state)))

(provide 'shenso-evil)
;;; shenso-evil.el ends here
