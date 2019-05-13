;; move custom data out of my init script dammit!
(setq custom-file "~/.emacs.d/custom.el")

;; load startup scripts
(add-to-list 'load-path "~/.emacs.d/elisp")
;;; initialize packaging before configuration
(load "init-packaging")
(load "init-appearance")
(load "init-editor")
(load "init-org")
