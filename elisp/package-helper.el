;;; package-helper.el by Shawn Henson

;; To the extent possible under law, the person who associated CC0 with
;; package-helper.el has waived all copyright and related or neighboring rights
;; to package-helper.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

(require 'dired)

(defun bootstrap-use-package (&optional emacs-data-dir)
  (defvar bootstrap-version)
  (setq straight-check-for-modifications '(check-on-save find-when-checking))

  (when emacs-data-dir
    (setq straight-base-dir emacs-data-dir)
    (let ((versions-dir (expand-file-name "versions/" user-emacs-directory))
          (straight-dir (expand-file-name "straight/" emacs-data-dir)))
      (unless (file-exists-p versions-dir)
        (make-directory versions-dir :parents))
      (unless (file-exists-p straight-dir)
        (make-directory straight-dir t))
      (make-symbolic-link versions-dir (expand-file-name "versions/" straight-dir) t)))

  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (load-file )
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package))

(provide 'package-helper)
