(setq custom-file (concat user-emacs-directory "custom.el"))
(setq scripts-directory (concat user-emacs-directory "elisp/"))

;; add scripts to load path
(add-to-list 'load-path scripts-directory)

;;; initialize packaging before configuration
(load "init/appearance")
(load "init/editor")
(load "init/org")
