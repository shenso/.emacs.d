;;; evil-dired.el by Shawn Henson -*- lexical-binding: t; -*-

;; To the extent possible under law, the person who associated CC0 with
;; evil-dired.el has waived all copyright and related or neighboring rights
;; to evil-dired.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;; Commentary:
;; Not meant to replace evil-collection's dired bindings, just enhance them.

(require 'evil)

(defun quit-window-and-kill-buffer ()
  (interactive)
  (quit-window t))

(defun evil-dired-setup ()
  (evil-define-key 'normal dired-mode-map
    "f" #'find-name-dired
    "p" #'find-lisp-find-dired
    "P" #'find-lisp-find-dired-other-window
    "n" #'find-lisp-find-dired-filter
    "F" #'find-grep-dired
    "q" #'quit-window-and-kill-buffer))

(provide 'evil-dired)
