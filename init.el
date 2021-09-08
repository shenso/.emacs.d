(setq custom-file "~/.emacs.d/custom.el")
(setq scripts-directory
      (concat user-emacs-directory "elisp/"))

;; load startup scripts
(add-to-list 'load-path scripts-directory)
(add-to-list 'load-path (concat scripts-directory "editor"))

;;; initialize packaging before configuration
(load "init/appearance")
(load "init/editor")
(load "init/org")
