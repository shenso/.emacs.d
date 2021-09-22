;; startup optimization
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda () setq gc-cons-threshold (expt 2 23)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(setq scripts-directory (concat user-emacs-directory "elisp/"))

;; add scripts to load path
(add-to-list 'load-path scripts-directory)

;;; initialize packaging before configuration
(load "init/appearance")
(load "init/editor")
(load "init/org")
