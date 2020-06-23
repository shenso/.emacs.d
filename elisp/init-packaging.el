(require 'package)

;;; melpa
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable
  ;; as desired
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable"
  ;; (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
		 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;; use-package
;; install use-package if needed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; make use-package install packages
(require 'use-package)
(require 'use-package-ensure)
;; auto-updating of packages
(use-package auto-package-update
  :config
  (setq auto-package-update-prompt-before-update t ;; choice to update
	auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))
