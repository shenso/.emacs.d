;;; dired-subtree.el --- Insert subdirectories in a tree-like fashion -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.1
;; Created: 25th February 2014
;; Package-Requires: ((dash "2.5.0") (dired-hacks-utils "0.0.1") (emacs "24.3"))
;; URL: https://github.com/Fuco1/dired-hacks

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; just adds a switch to the ls command for subdirectories to exclude "." and
;; "..".  Should only be used with GNU coreutils.

;;; Code:

(defun shenso-dired-subtree--readin (dir-name)
  (with-temp-buffer
    (insert-directory
     dir-name
     (string-replace "-a" "-A" dired-listing-switches)
     nil t)
    (delete-char -1)
    (goto-char (point-min))
    (delete-region
     (progn (beginning-of-line) (point))
     (progn (forward-line
             (if (save-excursion
                   (forward-line 1)
                   (end-of-line)
                   (looking-back "\\."))
                 3 1))
            (point)))
    (insert "  ")
    (while (= (forward-line) 0)
      (insert "  "))
    (delete-char -2)
    (buffer-string)))

(defun install-dired-subtree-hack()
  (advice-add 'dired-subtree--readin
              :override #'shenso-dired-subtree--readin))


(provide 'dired-subtree-hack)
;;; dired-subtree-hack.el ends here
