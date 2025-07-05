;;; package-helper.el --- Helpers to setup package management -*- lexical-binding: t -*-

;; To the extent possible under law, the person who associated CC0 with
;; package-helper.el has waived all copyright and related or neighboring rights
;; to package-helper.el.

;; You should have received a copy of the CC0 legalcode along with this
;; work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:

;; I'm a little unnerved that straight recommends bootstrapping with arbitrary
;; code execution from a Github URL.  It is worth investigating if there are
;; other package managers which can lock versions and also build from Github
;; repos adhoc.  May be worth just extending `use-package' to do this.

;; TODO: remove as many dependencies on MELPA as possible.  GNU and non-GNU ELPA
;;       should be prioritized.

;;; Code:

(require 'dired)

(defconst striaght-install-url
  (concat "https://raw.githubusercontent.com/radian-software/straight.el/"
          "develop/install.el"))
(defconst straight-bootstrap-rel-path
  "straight/repos/straight.el/bootstrap.el")

(defvar bootstrap-version 7)

(defun bootstrap-use-package (&optional emacs-data-dir)
  "Ensure installation of straight to use the `use-package' macro.
EMACS-DATA-DIR is the directory in which packages should be installed."
  (setq straight-check-for-modifications '(check-on-save find-when-checking))

  (when emacs-data-dir
    (setq straight-base-dir emacs-data-dir)
    (let ((versions-dir (expand-file-name "versions/" user-emacs-directory))
          (straight-dir (expand-file-name "straight/" emacs-data-dir)))
      (unless (file-exists-p versions-dir)
        (make-directory versions-dir :parents))
      (unless (file-exists-p straight-dir)
        (make-directory straight-dir t))
      ;; Straight does not allow customizing the versions path specifically, so
      ;; use a symbolic link to make it accessible to version control.
      (make-symbolic-link versions-dir
                          (expand-file-name "versions/" straight-dir) t)))

  (let ((bootstrap-file
         (expand-file-name
          straight-bootstrap-rel-path
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory))))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously

           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (setq use-package-compute-statistics t)
  (straight-use-package 'use-package))

(provide 'package-helper)

;;; package-helper.el ends here
